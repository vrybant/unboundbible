unit UnitNormalize;

interface

uses
  Classes, SysUtils, UnitType, UnitLib;

//function MybibleExStrongs(s: string; NewTestament: boolean): string;
  function Normalize(s: string; format: TFileFormat; purge: boolean = true): string;

implementation

function MybibleExStrongs(s: string; NewTestament: boolean): string;
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
        if not Prefix('H',List[i]) and not Prefix('G',List[i]) then
          if NewTestament then List[i] := 'G' + List[i]
                          else List[i] := 'H' + List[i];
    end;

  Result := ListToString(List);
end;

{ ----------------------------------------------------------------------------------------------- }

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

function Normalize(s: string; format: TFileFormat; purge: boolean = true): string;
var
  List : TStringArray;
begin
  List := XmlToList(s);

  if format = unbound then
    begin
      Strongs(List);
      Footnotes(List);
      PurgeTag(List,'<TS>','<Ts>');
      if purge then PurgeTag(List, '<RF','<Rf>');
    end;

  if format = mybible then
    begin
      PurgeTag(List,' <h>','</h>');
      if purge then PurgeTag(List,  '<f','</f>');
    end;

  Result := Trim(ListToString(List));
  Replace(Result,'</S><S>','</S> <S>');
end;

end.

