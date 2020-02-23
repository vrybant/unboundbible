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
//r := '\cf2_' + Tag + '\cf1_'; Result := r; exit; // show tags
  if jColor then color := '\cf7';

  if Tag =  '<J>' then begin r := '\cf7_'; jColor := true  end;
  if Tag = '</J>' then begin r := '\cf1_'; jColor := false end;

  if Tag =  '<l>' then r := '\cf3_';
  if Tag = '</l>' then r := '\cf1_';
  if Tag =  '<r>' then r := '\cf2_';
  if Tag = '</r>' then r := '\cf1_';
  if Tag =  '<n>' then r := '\cf5_';       // note
  if Tag = '</n>' then r := color + '_';
  if Tag =  '<v>' then r := '\cf5_';
  if Tag = '</v>' then r := color + '_';
  if Tag =  '<f>' then r := '\cf6\super_';
  if Tag = '</f>' then r := color + '\nosupersub_';
  if Tag =  '<m>' then r := '\cf5\super_';
  if Tag = '</m>' then r := color + '\nosupersub_';
  if Tag =  '<S>' then r := '\cf8\super_';
  if Tag = '</S>' then r := color + '\nosupersub_';
  if Tag =  '<i>' then r := '\cf5\i_';
  if Tag = '</i>' then r := color + '\i0_';

  Result := r;
end;

function ApplyHtml(Tag: string): string;
var
  r : string = '';
begin
  Tag := LowerCase(Tag);
  if Prefix('<a ', Tag) then Tag := '<a>';

  if Tag =   '<i>' then r := '\cf5\i_';
  if Tag =  '</i>' then r := '\cf1\i0_';
  if Tag =   '<a>' then r := '\cf5_';
  if Tag =  '</a>' then r := '\cf1_';
  if Tag =   '<b>' then r := '\cf8\b_' ;
  if Tag =  '</b>' then r := '\cf1\b0_';
  if Tag =   '<h>' then r := '\cf3_';
  if Tag =  '</h>' then r := '\cf1_';
  if Tag =  '</p>' then r := '\par_';
  if Tag =  '<br>' then r := '\par_';

  if Tag = '<tab>' then r := '\tab_';
  if Tag = '<rtl>' then r := '\rtlpar\qr_';
  if Tag = '<ltr>' then r := '\ltrpar\qr_';

  if Tag =  '<sup>' then r := '\super_';
  if Tag = '</sup>' then r := '\nosupersub_';

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
  item, p : string;
begin
  Result := '';
  if html then HtmlReplacement(s);
  List := XmlToList(s);

  for item in List do
    begin
      if Prefix('<', item) then p := Apply(item, html) else p := item;
      Result += p;
    end;

  RemoveDoubleSpace(Result);
  Replace(Result,'_',' ');
  Result := rtf_open(Font) + Trim(Result) + rtf_close;
end;

end.

