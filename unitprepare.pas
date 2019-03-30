unit UnitPrepare;

interface

uses
  Classes, SysUtils, UnitData, UnitLib;

function MybibleStrongsToUnbound(s: string; NewTestament: boolean): string;
function Prepare(s: string; format: TFileFormat; purge: boolean = true): string;

implementation

const
  Max = 11;
  Dictionary : array [1..Max,1..2] of string = (
    ('<FR>', '<J>'),
    ('<Fr>','</J>'),
    ('<FI>', '<i>'), // italic
    ('<Fi>','</i>'),
    ('<FO>', '<t>'), // quote
    ('<Fo>','</t>'),
    ('<TS>', '<h>'), // title
    ('<Ts>','</h>'),
    ('<RF>', '<f>'), // footnote
    ('<RF ', '<f '),
    ('<Rf>','</f>'));

procedure ReplaceMyswordTags(var s: string);
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
          l := true;
          Continue;
        end;

      if List[i] = '</S>' then l := false;

      if l then
        if NewTestament then List[i] := 'G' + List[i]
                        else List[i] := 'H' + List[i];
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
  Replace(s, '<f>','<f>✻[~');
  Replace(s,'</f>','~]</f>');
   CutStr(s,'[~','~]');
end;

procedure FootnotesEx(var s: string);
begin
  Replace(s, '<f ','<f><'  );
  Replace(s,'</f>','~]</f>');
  ExtractMarkers(s);
  CutStr(s,'[~','~]');
end;

function Prepare(s: string; format: TFileFormat; purge: boolean = true): string;
begin
  if format = mysword then
    begin
      if Pos('<W',s) > 0 then Strongs(s);
      ReplaceMyswordTags(s);
    end;

  if format in [unbound, mysword] then
    begin
      if not purge and (Pos('<f>',s) > 0) then Footnotes(s);
      if not purge and (Pos('<f ',s) > 0) then FootnotesEx(s);
    end;

  CutStr(s,'<h>','</h>');
  if purge then CutStr(s,'<f','</f>');
  if not purge then Replace(s,'</f><f>','</f> <f>');

  {$ifdef linux}
    Replace(s,'<S>',' <S>');
  {$else}
    Replace(s,'</S><S>','</S> <S>');
  {$endif}

  Result := s;
end;

end.

