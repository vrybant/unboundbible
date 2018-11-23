unit UnitParse;

interface

uses
  Classes, SysUtils, UnitLib;

function rtf_open: string;
function Parse(s: string; jtag: boolean = false): string;
function ParseHTML(s: string; tab: boolean = false): string;

const
  rtf_rtl = '\rtlpar\qr ';
  rtf_close = '}';

implementation

{ Rich Text Open }

function rtf_open: string;
begin
  Result :=
      '{\rtf1\ansi\ansicpg1251\cocoartf1187 '
    + '{\fonttbl'
    + '{\f0 default;}'
    + '{\f1 ' + DefaultFont.Name + ';}'
    + '}'
    + '{\colortbl;'
    + '\red0\green0\blue0;'        // 1 black
    + '\red192\green0\blue0;'      // 2 red
    + '\red0\green0\blue128;'      // 3 navy
    + '\red0\green128\blue0;'      // 4 green
    + '\red128\green128\blue128;'  // 5 gray
    + '\red0\green128\blue128;'    // 6 teal
    + '\red128\green0\blue0;'      // 7 maroon
    + '\red153\green102\blue51;'   // 8 brown
    + '\red139\green69\blue19;'    // 9 saddlebrown
//  + '\red128\green0\blue128;'    // 0 purple
    + '}'
    + '\f1\cf1'
    + '\fs' + ToStr(DefaultFont.Size * 2);
end;

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
  //Result := '\cf7 ' + s + '\cf1 '; Exit;  // show tags

  if jColor then color := '\cf7' else color := '\cf1';

  if j then if s =  '<J>' then begin r := '\cf7 '; jColor := true;  end;
  if j then if s = '</J>' then begin r := '\cf1 '; jColor := false; end;

  if s = '<RF>' then r := '\cf6 ';
  if s = '<Rf>' then r := color;
  if s =  '<S>' then r := '\cf8\super ';
  if s = '</S>' then r := color + '\nosupersub ';
  if s =  '<l>' then r := '\cf2 ';
  if s = '</l>' then r := '\cf1 ';
  if s =  '<n>' then r := '\cf5 ';       // note
  if s = '</n>' then r := color + ' ';
  if s =  '<m>' then r := '\cf9\super '; // morphology
  if s = '</m>' then r := color + '\nosupersub ';
  if s =  '<f>' then r := '\cf6\super ';
  if s = '</f>' then r := color + '\nosupersub ';

  s := LowerCase(s);

  if s =  '<i>' then r := '\cf5\i ';
  if s = '</i>' then r := color + '\i0 ';

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

