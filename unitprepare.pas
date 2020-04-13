unit UnitPrepare;

interface

uses
  Classes, SysUtils, UnitData, UmLib, UnitLib;

function Coercion(s: string; format: TFileFormat; nt: boolean): string;
function Preparation(s: string; format: TFileFormat; nt: boolean; purge: boolean = true): string;

implementation

const
  Max = 21;
  TagsDictionary : array [1..Max,1..2] of string = (
    ('<FR>', '<J>'),('<Fr>','</J>'),
    ('<-->', '<S>'),('<-->','</S>'), // strong
    ('<-->', '<m>'),('<-->','</m>'), // morphology
    ('<FI>', '<i>'),('<Fi>','</i>'), // italic
    ('<FO>', '<t>'),('<Fo>','</t>'), // quote
    ('<TS>', '<h>'),('<Ts>','</h>'), // title
    ('<E>',  '<n>'),('<e>', '</n>'), // english translation
    ('<T>',  '<n>'),('<t>', '</n>'), // translation
    ('<x>', '</x>'),('<X>',  '<x>'), // transliteration
    ('<RF>', '<f>'),('<RF ', '<f '), // footnote
    ('<Rf>','</f>'));

procedure ReplaceMyswordTags(var s: string);
var i : integer;
begin
  for i:=1 to Max do Replace(s, TagsDictionary[i,1], TagsDictionary[i,2]);
  Replace(s,'¶','');
end;

procedure ReplaceMybibleTags(var s: string);
begin
  Replace(s, '<t>', ' ');
  Replace(s,'</t>', ' ');
  Replace(s,'<pb/>',' ');
  Replace(s,'<br/>',' ');
end;

function EnabledTag(tag: string): boolean;
var i : integer;
begin
  Result := True;
  for i:=1 to Max do if Pos(TagsDictionary[i,2], tag) > 0 then Exit;
  Result := false;
end;

procedure CleanUnabledTags(var s: string);
var
  List : TStringArray;
  i : integer;
begin
  List := XmlToList(s);

  for i:=Low(List) to High(List) do
    if Prefix('<', List[i]) then
      if not EnabledTag(List[i]) then List[i] := '';

  s := Trim(ListToString(List));
  RemoveDoubleSpace(s);
end;

function MybibleStrongsToUnbound(s: string; NewTestament: boolean): string;
var symbol : string;
begin
  if NewTestament then symbol := 'G' else symbol := 'H';
  Replace(s, '<S>', '<S>' + symbol);
  Result := s;
end;

function MyswordStrongsToUnbound(var s: string): string;
var
  List : TStringArray;
  number : string;
  i : integer;
begin
  List := XmlToList(s);

  for i:=Low(List) to High(List) do
    if Prefix('<WH', List[i]) or Prefix('<WG', List[i]) then
      begin
        number := List[i];
        Replace(number,'<W','');
        Replace(number,'>' ,'');
        List[i] := '<S>' + number + '</S>';
      end
    else if Prefix('<WT', List[i]) then
      begin
        number := List[i];
        Replace(number,'<WT','');
        Replace(number,'>' ,'');
        List[i] := '<m>' + number + '</m>';
      end;

  Result := Trim(ListToString(List));
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

function Coercion(s: string; format: TFileFormat; nt: boolean): string;
begin
  if format = mysword then
    begin
      if Pos('<W',s) > 0 then s := MyswordStrongsToUnbound(s);
      ReplaceMyswordTags(s);
    end;

  if format = mybible then
    begin
      if Pos('<S>',s) > 0 then s := MybibleStrongsToUnbound(s, nt);
      ReplaceMybibleTags(s);
    end;

  CleanUnabledTags(s);
  RemoveDoubleSpace(s);
  Result := Trim(s);
end;

function Preparation(s: string; format: TFileFormat; nt: boolean; purge: boolean = true): string;
begin
  if format <> unbound then s := Coercion(s, format, nt);

  CutStr(s,'<h>','</h>');
  CutStr(s,'<x>','</x>');

  if format in [unbound, mysword] then
    begin
      if Pos('<f>',s) > 0 then Footnotes(s);
      if Pos('<f ',s) > 0 then FootnotesEx(s);
    end;

  if purge then CutStr(s,'<f>','</f>');

  {$ifdef linux}
    Replace(s,'><','>  <'); // ?
  {$else}
    Replace(s,'><','> <');
  {$endif}

  Result := s;
end;

end.

