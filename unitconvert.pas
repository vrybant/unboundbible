unit UnitConvert;

interface

uses
  Classes, SysUtils, UnitModule, UnitLib;

function ExtractMyswordFootnotes(s: string): TStringArray;
function ConvertTags(s: string; format: TFileFormat): string;
function Prepare(s: string; format: TFileFormat; purge: boolean = true): string;

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

function ExtractMyswordFootnotes(s: string): TStringArray;
var
  item : string;
  marker : string = '';
  asterisks : string = '';
  r : string = '';
  l : boolean = false;
begin
  Result := [];
  for item in XmlToList(s) do
    begin
      if item = '<Rf>' then
        begin
          if l then Result.Add(marker + delimiter + Trim(r));
          r := '';
          l := false;
        end;

      if l then r += item;

      if item = '<RF>' then
        begin
          asterisks += '*';
          marker := '[' + asterisks + ']';
          l := true;
        end;

      if Prefix('<RF q=',item) then
        begin
          marker := item.Replace('<RF q=','[').Replace('>',']');
          l := true;
        end;
    end;
end;

procedure SetAsterisks(var s: string);
var
  List : TStringArray;
  marker : string = '';
  i : integer;
begin
  List := XmlToList(s);
  for i:=Low(List) to High(List) do
    if List[i] = '<f>' then
      begin
        marker += '*';
        List[i] := List[i].Replace('<f>','<f q=' + marker + '>');
      end;
  s := ''.Join('',List);
end;

procedure ExtractMyswordMarkers(var s: string);
var
  List : TStringArray;
  i : integer;
begin
  List := XmlToList(s);
  for i:=Low(List) to High(List) do
    if Prefix('<q=', List[i]) then
      List[i] := List[i].Replace('<q=','[').Replace('>' ,'][~');
  s := ''.Join('',List);
end;

procedure CutMyswordFootnotes(var s: string);
begin
  if s.Contains('<f>') then SetAsterisks(s);

  if s.Contains('<f ') then
    begin
      Replace(s, '<f ','<f><'  );
      Replace(s,'</f>','~]</f>');
      ExtractMyswordMarkers(s);
      CutStr(s,'[~','~]');
    end;
end;

procedure ReplaceMyswordTags(var s: string);
var i : integer;
begin
  MyswordStrongsToUnbound(s);
  for i:=1 to Max do Replace(s, TagsDictionary[i,1], TagsDictionary[i,2]);
  CutMyswordFootnotes(s);
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

  s := ''.Join('',List);
end;

function ConvertTags(s: string; format: TFileFormat): string;
begin
  if format = mysword then ReplaceMyswordTags(s);
  if format = mybible then ReplaceMybibleTags(s);

  CleanUnabledTags(s);
  RemoveDoubleSpaces(s);
  Result := Trim(s);
end;

function Prepare(s: string; format: TFileFormat; purge: boolean = true): string;
begin
  if format <> unbound then s := ConvertTags(s, format);

  if purge then CutStr(s,'<f>','</f>');
  CutStr(s,'<h>','</h>');

  Replace(s,'><', {$ifdef linux} '>  <' {$else} '> <' {$endif} );
  Result := s;
end;

end.

