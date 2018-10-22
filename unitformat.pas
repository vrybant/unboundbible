unit UnitFormat;

interface

uses
  Classes, SysUtils, UnitLib;


implementation

procedure ClearTitles(var List: TStringArray);
var
  l : boolean = false;
  i : integer;
begin
  for i:=Low(List) to High(List) do
    begin
      if List[i] = '<TS>' then l := true;

      if List[i] = '<Ts>' then
        begin
          List[i] := '';
          l := false;
        end;

      if l then List[i] := '';
    end;
end;

procedure RemakeFootnotes1(var List: TStringArray);
var
  l : boolean = false;
  i : integer;
begin
  l := false;
  for i:=Low(List) to High(List) do
    begin
      if List[i] = '<RF>' then
        begin
          l := true;
          Continue;
        end;

      if List[i] = '<Rf>' then
        begin
          l := false;
          Continue;
        end;

      if l then List[i] := '*';
    end;
end;

function ExtractMarker(s: string): string;
var
  x1, x2 : integer;
begin
  Result := s;
  x1 := Pos('=',s); if x1 = 0 then Exit;
  x2 := Pos('>',s); if x2 = 0 then Exit;
  Result := Copy(s,x1+1,x2-x1-1);
end;

procedure RemakeFootnotes2(var List: TStringArray);
var
  marker : string = '';
  l : boolean = false;
  i : integer;
begin
  for i:=Low(List) to High(List) do
    begin
      if Prefix('<RF ', List[i]) then
        begin
          marker := ExtractMarker(List[i]);
          List[i] := '<RF>';
          l := true;
          Continue;
        end;

      if List[i] = '<Rf>' then
        begin
          l := false;
          Continue;
        end;

      if marker <> '' then
        begin
          List[i] := ' ' + marker + ' ';
          marker := '';
          Continue;
        end;

      if l then List[i] := '';
    end;
end;

procedure Remake(var List: TStringArray);
begin
  ClearTitles(List);
  RemakeFootnotes1(List);
  RemakeFootnotes2(List);
end;

end.

