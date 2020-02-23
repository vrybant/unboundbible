unit UnitPrepare;

interface

uses
  Classes, SysUtils, UnitData, UmLib, UnitLib;

function Convert(s: string; format: TFileFormat; nt: boolean): string;
function Prepare(s: string; format: TFileFormat; purge: boolean = true): string;

implementation

const
  Max = 21;
  TagsDictionary : array [1..Max,1..2] of string = (
    ('<FR>', '<J>'),
    ('<Fr>','</J>'),
    ('<-->', '<S>'), // strong
    ('<-->','</S>'),
    ('<-->', '<m>'), // morphology
    ('<-->','</m>'),
    ('<FI>', '<i>'), // italic
    ('<Fi>','</i>'),
    ('<FO>', '<t>'), // quote
    ('<Fo>','</t>'),
    ('<TS>', '<h>'), // title
    ('<Ts>','</h>'),
    ('<E>',  '<n>'), // english translation
    ('<e>', '</n>'),
    ('<T>',  '<n>'), // translation
    ('<t>', '</n>'),
    ('<x>', '</x>'), // transliteration
    ('<X>',  '<x>'),
    ('<RF>', '<f>'), // footnote
    ('<RF ', '<f '),
    ('<Rf>','</f>'));

procedure ReplaceMyswordTags(var s: string);
var i : integer;
begin
  for i:=1 to Max do Replace(s, TagsDictionary[i,1], TagsDictionary[i,2]);
end;

function EnabledTag(tag: string): boolean;
var i : integer;
begin
  Result := True;
  for i:=1 to Max do if tag = TagsDictionary[i,2] then Exit;
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
  output(s);
  CutStr(s,'[~','~]');
end;

function Convert(s: string; format: TFileFormat; nt: boolean): string;
begin
  if format = mysword then
    begin
      if Pos('<W',s) > 0 then s := MyswordStrongsToUnbound(s);
      ReplaceMyswordTags(s);
    end;

  if format = mybible then
    begin
      if Pos('<S>',s) > 0 then s := MybibleStrongsToUnbound(s, nt);
      CutStr(s,'<f','</f>');
      Replace(s, '<t>', ' ');
      Replace(s,'</t>', ' ');
      Replace(s,'<pb/>',' ');
      Replace(s,'<br/>',' ');
    end;

//CutStr(s,'<S>','</S>');
  RemoveDoubleSpace(s);

  Result := Trim(s);
end;

function Prepare(s: string; format: TFileFormat; purge: boolean = true): string;
begin
  if format = mysword then
    begin
      if Pos('<W',s) > 0 then s := MyswordStrongsToUnbound(s);
      ReplaceMyswordTags(s);
      Replace(s,'¶','');
    end;

  if format in [unbound, mysword] then
    begin
      if Pos('<f>',s) > 0 then Footnotes(s);
      if Pos('<f ',s) > 0 then FootnotesEx(s);
    end;

  CutStr(s,'<h>','</h>');
  CutStr(s,'<x>','</x>');

  if purge then CutStr(s,'<f>','</f>');
  CleanUnabledTags(s);

  {$ifdef linux}
    Replace(s,'><','>  <'); // ?
  {$else}
    Replace(s,'><','> <');
  {$endif}

  Result := s;
end;

end.

