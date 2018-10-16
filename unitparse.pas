unit UnitParse;

interface

uses
  Classes, SysUtils, UnitLib;

function Parse(st: string; jtag: boolean = false): string;
function ParseHTML(st: string): string;

implementation

procedure XmlToList(s: string; List: TStringList);
var
  temp : string = '';
  c : char;
begin
  for c in s do
    begin
      if c = '<' then
        begin
          List.Add(temp);
          temp := '';
        end;

      temp := temp + c;

      if c = '>' then
        begin
          List.Add(temp);
          temp := '';
        end;
    end;

  if temp <> '' then List.Add(temp);
end;

procedure Clean(List: TStringList);
var
  l : boolean = false;
  i : integer;
begin
  for i:=0 to List.Count-1 do
    begin
      if List[i] = '<TS>' then l := true;
      if List[i] = '<Ts>' then l := false;
      if List[i] = '<RF>' then l := true;
      if List[i] = '<Rf>' then l := false;
      if Prefix('<RF ', List[i]) then l := true;
      if l then List[i] := '  ';
    end;
end;

function Format(s: string; j: boolean): string;
var
  r : string = '';
  color : string;
const
  jColor : boolean = false;
begin
  if jColor then color := '\cf2' else color := '\cf1';

  if s =  '<FR>' then s :=  '<J>';
  if s =  '<Fr>' then s := '</J>';
  if s =  '<FI>' then s :=  '<i>';
  if s =  '<Fi>' then s := '</i>';
  if s =  '<em>' then s :=  '<i>';
  if s = '</em>' then s := '</i>';

  if j then if s =  '<J>' then begin r := '\cf2 '; jColor := true;  end;
  if j then if s = '</J>' then begin r := '\cf1 '; jColor := false; end;

  s := LowerCase(s);

  if s =  '<i>' then r := '\i ';
  if s = '</i>' then r := '\i0 ';
  if s =  '<s>' then r := '\cf6\super ';
  if s = '</s>' then r := color + '\nosupersub ';
  if s =  '<f>' then r := '\cf5\super ';
  if s = '</f>' then r := color + '\nosupersub ';

  Result := r;
end;

function HTML(s: string): string;
var
  r : string = '';
begin
  s := LowerCase(s);

  if Prefix('<p ', s) then s := '<p>';
  if Prefix('<a ', s) then s := '<a>';

  if s = '<br/>' then s := '<br>';
  if s =  '<p/>' then s := '<p>';
  if s =  '<td>' then s := '<br>';
  if s =  '<tr>' then s := '<br>';
  if s = '</td>' then s := '<br>';
  if s = '</tr>' then s := '<br>';

  if s =  '<b>' then r := '\b ' ;
  if s = '</b>' then r := '\b0 ';
  if s =  '<i>' then r := '\i ';
  if s = '</i>' then r := '\i0 ';
  if s = '<br>' then r := '\par\tab ';
  if s =  '<p>' then r := '\par\tab ';
  if s = '</p>' then r := '\par ';
  if s =  '<a>' then r := '\cf6 ';
  if s = '</a>' then r := '\cf1 ';
  if s =  '<strong>' then r := '\b ' ;
  if s = '</strong>' then r := '\b0 ';
  if s =  '<sup>' then r := '\super ';
  if s = '</sup>' then r := '\nosupersub ';

  Result := r;
end;

function Parse(st: string; jtag: boolean = false): string;
var
  List : TStringList;
  s : string;
begin
  Result := '';
  Replace(st,'</S><S>','</S> <S>');

  List := TStringList.Create;
  XmlToList(st, List);
  Clean(List);

  for s in List do
    if Prefix('<',s) then Result := Result + Format(s, jtag)
                     else Result := Result + s;

  Result := DelDoubleSpace(Trim(Result));
  List.Free;
end;

function ParseHTML(st: string): string;
var
  List : TStringList;
  s : string;
begin
  Result := '';

  List := TStringList.Create;
  XmlToList(st, List);

  for s in List do
    if Prefix('<',s) then Result := Result + HTML(s)
                     else Result := Result + s;
  List.Free;
end;

end.

