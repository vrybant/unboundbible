unit UnitPrepare;

interface

uses
  Classes, SysUtils, UnitType, UnitLib;

function MybibleStrongsToUnbound(s: string; NewTestament: boolean): string;
function Prepare(s: string; format: TFileFormat; purge: boolean = true): string;

implementation

function MybibleStrongsToUnbound(s: string; NewTestament: boolean): string;
var
  List : TStringArray;
  l : boolean = false;
  i : integer;
begin
  List := XmlToList(s);

  for i:=Low(List) to High(List) do
    begin
      if List[i] = '<S>' then
        begin
          List[i] := '<';
          l := true;
          Continue;
        end;

      if List[i] = '</S>' then
        begin
          List[i] := '>';
          l := false;
        end;

      if l then
        if NewTestament then List[i] := 'WG' + List[i]
                        else List[i] := 'WH' + List[i];
    end;

  Result := ListToString(List);
end;

function ReplaceTag(s: string): string;
var w : string;
begin
  w := LowerCase(s);
  if s =   '<J>' then s := '<FR>';
  if s =  '</J>' then s := '<Fr>';
  if s =   '<t>' then s := '<FO>'; // quote
  if s =  '</t>' then s := '<Fo>';
  if s =   '<h>' then s := '<TS>'; // title
  if s =  '</h>' then s := '<Ts>';
  if s =   '<f>' then s := '<RF>'; // footnote
  if s =  '</f>' then s := '<Rf>';
  if w =   '<i>' then s := '<FI>';
  if w =  '</i>' then s := '<Fi>';
  if w =  '<em>' then s := '<FI>';
  if w = '</em>' then s := '<Fi>';

  if w = '<pb/>' then s := ''; // unused tags
  if w = '<br/>' then s := '';

  Result := s;
end;

procedure ReplaceTags(const List: TStringArray);
var i : integer;
begin
  for i:= Low(List) to High(List) do List[i] := ReplaceTag(List[i]);
end;

function ExtractStrongNumber(s: string): string;
var
  x1, x2 : integer;
begin
  Result := s;
  x1 := Pos('<',s); if x1 = 0 then Exit;
  x2 := Pos('>',s); if x2 = 0 then Exit;
  Result := Copy(s,x1+2,x2-x1-2);
end;

procedure Strongs(const List: TStringArray);
var
  number : string = '';
  i : integer;
begin
  for i:=Low(List) to High(List) do
    if Prefix('<W', List[i]) then
      begin
        number := ExtractStrongNumber(List[i]);
        List[i] := '<S>' + number + '</S>';
      end;
end;

function ExtractFootnoteMarker(s: string): string;
var
  x1, x2 : integer;
begin
  Result := s;
  x1 := Pos('=',s); if x1 = 0 then Exit;
  x2 := Pos('>',s); if x2 = 0 then Exit;
  Result := Copy(s,x1+1,x2-x1-1);
end;

procedure Footnotes(const List: TStringArray);
var
  marker : string = '';
  l : boolean = false;
  i : integer;
begin
  for i:=Low(List) to High(List) do
    begin
      if List[i] = '<RF>' then
        begin
          marker := '*';
          l := true;
          Continue;
        end;

      if Prefix('<RF ', List[i]) then
        begin
          marker := ExtractFootnoteMarker(List[i]);
          List[i] := '<RF>';
          l := true;
          Continue;
        end;

      if l and (List[i] = '<Rf>') then
        begin
          l := false;
          Continue;
        end;

      if marker <> '' then
        begin
          List[i] := marker;
          marker := '';
          Continue;
        end;

      if l then List[i] := '';
    end;
end;

function Prepare(s: string; format: TFileFormat; purge: boolean = true): string;
var
  List : TStringArray;
begin
  List := XmlToList(s);

  if format = unbound then
    begin
      Strongs(List);
      Footnotes(List);
    end;

  ReplaceTags(List);
  PurgeTag(List,'<TS>','<Ts>');
  if purge then PurgeTag(List, '<RF','<Rf>');

  Result := Trim(ListToString(List));
  Replace(Result,'</S><S>','</S> <S>');
end;

end.

