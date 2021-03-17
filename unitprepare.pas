unit UnitPrepare;

interface

uses
  Classes, SysUtils, UnitData, UnitLib;

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
    ('<X>',  '<m>'),('<x>', '</m>'), // transliteration
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
  for i:=1 to Max do if Prefix(TagsDictionary[i,2], tag) then Exit(True);
  Result := False;
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

  s := ''.Join('',List).Trim;
  RemoveDoubleSpaces(s);
end;

procedure MyswordStrongsToUnbound(var s: string);
var
  List : TStringArray;
  number : string;
  i : integer;
begin
  if not s.Contains('<W') then Exit;
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

  s := ''.Join('',List).Trim;
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
  s := ''.Join('',List);
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
      MyswordStrongsToUnbound(s);
      ReplaceMyswordTags(s);
    end;

  if format = mybible then ReplaceMybibleTags(s);

  CleanUnabledTags(s);
  Result := Trim(s);
end;

function Preparation(s: string; format: TFileFormat; nt: boolean; purge: boolean = true): string;
begin
  if format <> unbound then s := Coercion(s, format, nt);

  if format in [unbound, mysword] then
    begin
      if s.Contains('<f>') then Footnotes(s);
      if s.Contains('<f ') then FootnotesEx(s);
    end;

  if purge then CutStr(s,'<f>','</f>');
  CutStr(s,'<h>','</h>');

  Replace(s,'><', {$ifdef linux} '>  <' {$else} '> <' {$endif} );
  Result := s;
end;

end.

