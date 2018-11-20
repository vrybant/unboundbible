unit UnitReform;

interface

uses
  Classes, SysUtils, UnitLib;

function Reform(s: string; purge: boolean = true): string;

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

function ExtractFootnoteMarker(s: string): string;
var
  x1, x2 : integer;
begin
  Result := s;
  x1 := Pos('=',s); if x1 = 0 then Exit;
  x2 := Pos('>',s); if x2 = 0 then Exit;
  Result := Copy(s,x1+1,x2-x1-1);
end;

procedure Footnotes(const List: TStringArray; p: boolean);
var
  marker : string = '';
  l : boolean = false;
  i : integer;
begin
  for i:=Low(List) to High(List) do
    begin
      if List[i] = '<RF>' then
        begin
          if not p then marker := '*';
          if p then List[i] := '';
          l := true;
          Continue;
        end;

      if Prefix('<RF ', List[i]) then
        begin
          if not p then marker := ExtractFootnoteMarker(List[i]);
          if p then List[i] := '' else List[i] := '<RF>';
          l := true;
          Continue;
        end;

      if l and (List[i] = '<Rf>') then
        begin
          if p then List[i] := '';
          l := false;
          Continue;
        end;

      if marker <> '' then
        begin
          List[i] := ' ' + marker;
          marker := '';
          Continue;
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
        List[i] := ' ' + '<S>' + number + '</S>';
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
      if List[i] =  '<RF>' then List[i] :=  '<f>';
      if List[i] =  '<Rf>' then List[i] := '</f>';
      if List[i] =  '<em>' then List[i] :=  '<i>';
      if List[i] = '</em>' then List[i] := '</i>';
    end;
end;

function Reform(s: string; purge: boolean = true): string;
var
  List : TStringArray;
begin
  Replace(s,'</S><S>','</S> <S>');
  List := XmlToList(s);

  RemoveTagContent(List,'<TS>','<Ts>'); // Prologue
  Footnotes(List, purge);
  Strongs(List);
  Replacement(List);
  if purge then RemoveTagContent(List, '<f>','</f>');

  Result := Trim(ListToString(List));
end;

end.

