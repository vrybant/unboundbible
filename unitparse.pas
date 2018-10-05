unit UnitParse;

interface

uses
  Classes, SysUtils, UnitLib;

function Parse(st: string; jtag: boolean): string;

implementation

function XmlToList(s: string): TStringArray;
var
  List : TStringList;
  temp : string;
  i : integer;
begin
  List := TStringList.Create;
  temp := '';

  for i:=1 to Length(s) do
    begin
      if s[i] = '<' then
        begin
          List.Add(temp);
          temp := '';
        end;

      temp := temp + s[i];

      if s[i] = '>' then
        begin
          List.Add(temp);
          temp := '';
        end;
    end;

  if temp <> '' then List.Add(temp);

  Result := ListToArray(List);
  List.Free;
end;

function Format(s: string; j: boolean): string;
var
  r, color : string;
const
  jColor : boolean = false;
begin
  r := '';
  if jColor then color := '\cf2' else color := '\cf1';

  if s = '<FR>' then s :=  '<J>';
  if s = '<Fr>' then s := '</J>';
  if s = '<FI>' then s :=  '<i>';
  if s = '<Fi>' then s := '</i>';

  if j then if s =  '<J>' then begin r := '\cf2 '; jColor := true;  end;
  if j then if s = '</J>' then begin r := '\cf1 '; jColor := false; end;

  if s =  '<S>' then r := '\cf6\super ';
  if s = '</S>' then r := color + '\nosupersub ';
  if s =  '<f>' then r := '\cf5\super ';
  if s = '</f>' then r := color + '\nosupersub ';
  if s =  '<i>' then r := '\i ';
  if s = '</i>' then r := '\i0 ';
  if s =  '<h>' then r := '\cf2 ';
  if s = '</h>' then r := '\cf1 ';

  Result := r;
end;

function Parse(st: string; jtag: boolean): string;
var
  List : TStringArray;
  s : string;
begin
  Result := '';
  Replace(st, '</S><S>','</S> <S>');
  List := XmlToList(st);

  for s in List do
    if Prefix('<',s) then
      Result := Result + Format(s, jtag)
    else
      Result := Result + s;
end;

end.

