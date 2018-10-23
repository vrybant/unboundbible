unit UnitParse;

interface

uses
  Classes, SysUtils, UnitLib;

function Parse(s: string; jtag: boolean = false): string;
function ParseHTML(s: string): string;

implementation

function ParseString(s: string; j: boolean): string;
var
  r : string = '';
  color : string;
const
  jColor : boolean = false;
begin
 // r := '\cf4 ' + s + '\cf1 ';  // show tags

  if jColor then color := '\cf2' else color := '\cf1';

  if s =  '<FR>' then s :=  '<J>';
  if s =  '<Fr>' then s := '</J>';
  if s =  '<FI>' then s :=  '<i>';
  if s =  '<Fi>' then s := '</i>';
  if s =  '<RF>' then s :=  '<f>';
  if s =  '<Rf>' then s := '</f>';
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

procedure Replacement(var s: string);
begin
  Replace(s,'&nbsp;' ,' ');
  Replace(s,'&ldquo;','"');
  Replace(s,'&rdquo;','"');
end;

function Parse(s: string; jtag: boolean = false): string;
var
  List : TStringArray;
  i : integer;
begin
  List := XmlToList(s);

  for i:=Low(List) to High(List) do
    if Prefix('<', List[i]) then List[i] := ParseString(List[i], jtag);

  Result := Trim(ListToString(List));
end;

function ParseHTML(s: string): string;
var
  List : TStringArray;
  i : integer;
  l : boolean;
begin
  Replacement(s);
  List := XmlToList(s);
  l := Pos('<p>',LowerCase(s)) > 0;

  for i:=Low(List) to High(List) do
    if Prefix('<', List[i]) then List[i] := HTML(List[i]);

  Result := Trim(ListToString(List));
  if not l then Result := '\tab ' + Result;
end;

end.

