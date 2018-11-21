unit UnitReform;

interface

uses
  Classes, SysUtils, UnitType, UnitLib;

function Reform(s: string; format: TFileFormat; purge: boolean = true): string;

implementation

procedure RemoveTagContent(const List: TStringArray; StartTag, EndTag: string);
var
  l : boolean = false;
  i : integer;
begin
  for i:=Low(List) to High(List) do
    begin
      if List[i] = StartTag then l := true;

      if List[i] = EndTag then
        begin
          List[i] := '';
          l := false;
        end;

      if l then List[i] := '';
    end;
end;

function ExtractStrongNumber(s: string): string;
var
  x1, x2 : integer;
begin
  Result := s;
  x1 := Pos('<',s); if x1 = 0 then Exit;
  x2 := Pos('>',s); if x2 = 0 then Exit;
  Result := Copy(s,x1+2,x2-x1-3);
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
      if List[i] = '<f>' then
        begin
          marker := '*';
          l := true;
          Continue;
        end;

      if Prefix('<f ', List[i]) then
        begin
          marker := ExtractFootnoteMarker(List[i]);
          List[i] := '<f>';
          l := true;
          Continue;
        end;

      if l and (List[i] = '</f>') then
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

procedure Replacement(const List: TStringArray);
var i : integer;
begin
  for i:=Low(List) to High(List) do
    begin
      if List[i] =  '<FR>' then List[i] :=  '<J>';
      if List[i] =  '<Fr>' then List[i] := '</J>';
      if List[i] =  '<FI>' then List[i] :=  '<i>';
      if List[i] =  '<Fi>' then List[i] := '</i>';
      if List[i] =  '<FO>' then List[i] :=  '<t>'; // quote
      if List[i] =  '<Fo>' then List[i] := '</t>';
      if List[i] =  '<RF>' then List[i] :=  '<f>'; // footnote
      if List[i] =  '<Rf>' then List[i] := '</f>';
      if List[i] =  '<TS>' then List[i] :=  '<h>'; // prologue
      if List[i] =  '<Ts>' then List[i] := '</h>';
      if List[i] =  '<em>' then List[i] :=  '<i>';
      if List[i] = '</em>' then List[i] := '</i>';

      if List[i] =  '<CM>' then List[i] := '';
      if List[i] = '<pb/>' then List[i] := '';

      if Prefix('<RF ',List[i]) then Replace(List[i],'<RF','<f');
    end;
end;

function Reform(s: string; format: TFileFormat; purge: boolean = true): string;
var
  List : TStringArray;
begin
  List := XmlToList(s);

  Strongs(List);
  Replacement(List);
  if format = unbound then Footnotes(List);

  RemoveTagContent(List,'<h>','</h>');
  if purge then RemoveTagContent(List, '<f>','</f>');

  Result := Trim(ListToString(List));
  Replace(Result,'</S><S>','</S> <S>');
end;

end.

