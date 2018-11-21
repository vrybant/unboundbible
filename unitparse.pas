unit UnitParse;

interface

uses
  Classes, SysUtils, UnitLib;

function Parse(s: string; jtag: boolean = false): string;
function ParseHTML(s: string; tab: boolean = false): string;

implementation

function LeadingTab(s: string): string;
var x : integer;
begin
  Result := '\tab ';
  x := Pos('\par',s);
  if x = 0 then Exit;
  s := Copy(s,1,x);
  x := Pos('\tab',s);
  if x > 0 then Result := '';
end;

function ParseString(s: string; j: boolean): string;
var
  r : string = '';
  color : string;
const
  jColor : boolean = false;
begin
 // r := '\cf4 ' + s + '\cf1 ';  // show tags

  if jColor then color := '\cf7' else color := '\cf1';

  if j then if s =  '<J>' then begin r := '\cf7 '; jColor := true;  end;
  if j then if s = '</J>' then begin r := '\cf1 '; jColor := false; end;

  s := LowerCase(s);

  if s =  '<i>' then r := '\cf5\i ';
  if s = '</i>' then r := color + '\i0 ';
  if s =  '<s>' then r := '\cf8\super ';
  if s = '</s>' then r := color + '\nosupersub ';
  if s =  '<n>' then r := '\cf5 ';       // note
  if s = '</n>' then r := color + ' ';
  if s =  '<m>' then r := '\cf9\super '; // morphology
  if s = '</m>' then r := color + '\nosupersub ';
  if s =  '<f>' then r := '\cf6\super ';
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
  if s = '<br>' then r := '\par'; ///////// \tab ';
  //////// if s =  '<p>' then r := '\tab ';
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
  Replace(s,'&quot;' ,'"');
  Replace(s,'&ldquo;','"');
  Replace(s,'&rdquo;','"');
  Replace(s, #09,' ');
  DelDoubleSpace(s);
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

function ParseHTML(s: string; tab: boolean = false): string;
var
  List : TStringArray;
  i : integer;
begin
  Replacement(s);
  List := XmlToList(s);

  for i:=Low(List) to High(List) do
    if Prefix('<', List[i]) then List[i] := HTML(List[i]);

  Result := Trim(ListToString(List));
  if tab then Result := LeadingTab(Result) + Result;
end;

end.

