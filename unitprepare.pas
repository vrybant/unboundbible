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

procedure Strongs(var s: string);
var
  List : TStringArray;
  number : string;
  i : integer;
begin
  List := XmlToList(s);
  for i:=Low(List) to High(List) do
    if Prefix('<W', List[i]) then
      begin
        number := List[i];
        Replace(number,'<W','');
        Replace(number,'>' ,'');
        List[i] := '<S>' + number + '</S>';
      end;
  s := Trim(ListToString(List));
end;

procedure ExtractMarkers(var s: string);
var
  List : TStringArray;
  marker : string;
  i : integer;
begin
  List := XmlToList(s);
  for i:=Low(List) to High(List) do
    if Prefix('<q=', List[i]) then
      begin
        marker := List[i];
        Replace(marker,'<q=','');
        Replace(marker,'>' ,'');
        List[i] := marker + ' [~';
      end;
  s := ListToString(List);
end;

procedure Footnotes(var s: string);
begin
  Replace(s,'<RF>','<RF>✻ [~');
  Replace(s,'<Rf>','~]<Rf>');
   CutStr(s,'[~','~]');
end;

procedure FootnotesEx(var s: string);
begin
  Replace(s,'<RF ','<RF><');
  Replace(s,'<Rf>','~]<Rf>');
  ExtractMarkers(s);
  CutStr(s,'[~','~]');
end;

function Prepare(s: string; format: TFileFormat; purge: boolean = true): string;
begin
  if format = unbound then
    begin
      if Pos('<W',s) > 0 then Strongs(s);
      if not purge and (Pos('<RF>',s) > 0) then Footnotes(s);
      if not purge and (Pos('<RF ',s) > 0) then FootnotesEx(s);
    end;

  if format = mybible then ReplaceTags(s);

  CutStr(s,'<TS>','<Ts>');
  if purge then CutStr(s,'<RF','<Rf>');
  Replace(s,'</S><S>','</S> <S>');

  Result := s;
end;

end.

