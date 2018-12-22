unit UnitPrepare;

interface

uses
  Classes, SysUtils, UnitType, UnitLib;

function MybibleStrongsToUnbound(s: string; NewTestament: boolean): string;
function Prepare(s: string; format: TFileFormat; purge: boolean = true): string;

implementation

const
  Max = 8;
  Dictionary : array [1..Max,1..2] of string = (
    (  '<J>','<FR>'),
    ( '</J>','<Fr>'),
    (  '<t>','<FO>'), // quote
    ( '</t>','<Fo>'),
    (  '<h>','<TS>'), // title
    ( '</h>','<Ts>'),
    (  '<f>','<RF>'), // footnote
    ( '</f>','<Rf>'));

procedure ReplaceTags(var s: string);
var i : integer;
begin
  for i:=1 to Max do Replace(s, Dictionary[i,1], Dictionary[i,2]);
end;

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

procedure Strongs(const List: TStringArray);
var
  number : string;
  i : integer;
begin
  for i:=Low(List) to High(List) do
    if Prefix('<W', List[i]) then
      begin
        number := List[i];
        Replace(number,'<W','');
        Replace(number,'>' ,'');
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
      if Pos('<W' ,s) > 0 then Strongs(List);
      if Pos('<RF',s) > 0 then Footnotes(List);
    end;

  Result := Trim(ListToString(List));
  if format = mybible then ReplaceTags(Result);

  CutStr(Result,'<TS>','<Ts>');
  if purge then CutStr(Result, '<RF','<Rf>');

  Replace(Result,'</S><S>','</S> <S>');
end;

end.

