unit UmParseWin;

interface

uses
  Classes, SysUtils, Graphics, UmLib;

function ParseWin(s: string; Font: TFont; html: boolean = false): string;

implementation

uses UmParse;

const
  rtf_close = '}';

function rtf_open(Font: TFont): string;
begin
  Result :=
      '{\rtf1\ansi\ansicpg1251\cocoartf1187 '
    + '{\fonttbl'
    + '{\f0 default;}'
    + '{\f1 ' + Font.Name + ';}'
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
    + '\red128\green0\blue128;'    // 10 purple
    + '}'
    + '\f1\fs' + ToStr(Font.Size * 2)
    + '\cf1 ';
end;

function ApplyString(Tag: string): string;
var
  color : string = '\cf1';
  r : string = '';
const
  jColor : boolean = false;
begin
//r := '\cf2 ' + Tag + '\cf1 '; Result := r; exit; // show tags
  if jColor then color := '\cf7';

  if Tag =  '<J>' then begin r := '\cf7 '; jColor := true  end;
  if Tag = '</J>' then begin r := '\cf1 '; jColor := false end;

  if Tag =  '<l>' then r := '\cf3 ';
  if Tag = '</l>' then r := '\cf1 ';
  if Tag =  '<r>' then r := '\cf2 ';
  if Tag = '</r>' then r := '\cf1 ';
  if Tag =  '<n>' then r := '\cf5 ';       // note
  if Tag = '</n>' then r := color + ' ';
  if Tag =  '<v>' then r := '\cf5 ';
  if Tag = '</v>' then r := color + ' ';
  if Tag =  '<f>' then r := '\cf6\super ';
  if Tag = '</f>' then r := color + '\nosupersub ';
  if Tag =  '<m>' then r := '\cf5\super '; // morphology
  if Tag = '</m>' then r := color + '\nosupersub ';
  if Tag =  '<S>' then r := '\cf8\super ';
  if Tag = '</S>' then r := color + '\nosupersub ';
  if Tag =  '<i>' then r := '\cf5\i ';
  if Tag = '</i>' then r := color + '\i0 ';

  Result := r;
end;

function ApplyHtml(Tag: string): string;
var
  r : string = '';
begin
  Tag := LowerCase(Tag);
  if Prefix('<a ', Tag) then Tag := '<a>';

  if Tag =   '<i>' then r := '\cf5\i ';
  if Tag =  '</i>' then r := '\cf1\i0 ';
  if Tag =   '<a>' then r := '\cf5 ';
  if Tag =  '</a>' then r := '\cf1 ';
  if Tag =   '<b>' then r := '\cf8\b ' ;
  if Tag =  '</b>' then r := '\cf1\b0 ';
  if Tag =   '<h>' then r := '\cf3 ';
  if Tag =  '</h>' then r := '\cf1 ';
  if Tag =  '</p>' then r := '\par ';
  if Tag =  '<br>' then r := '\par ';

  if Tag = '<tab>' then r := '\tab ';
  if Tag = '<rtl>' then r := '\rtlpar\qr ';
  if Tag = '<ltr>' then r := '\ltrpar\qr ';

  if Tag =  '<sup>' then r := '\super ';
  if Tag = '</sup>' then r := '\nosupersub ';

  Result := r;
end;

function Apply(Tag: string; html: boolean): string;
begin
  Result := '';
  if not html then Result := ApplyString(Tag);
  if Result = '' then Result := ApplyHtml(Tag);
end;

function ParseWin(s: string; Font: TFont; html: boolean = false): string;
var
  List : TStringArray;
  i : integer;
begin
  if html then HtmlReplacement(s);
  List := XmlToList(s);

  for i:=Low(List) to High(List) do
    if Prefix('<', List[i]) then
      List[i] := Apply(List[i], html);

  Result := Trim(ListToString(List));
  Result := rtf_open(Font) + Result + rtf_close;
end;

end.

