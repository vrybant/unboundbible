unit UnitLang;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, IniFiles,  ClipBrd, StrUtils, UnitLib, LCLProc;

type
  TCharset = record
    n : string[20];   // Name
    v : TFontCharset; // Value
  end;

  TLang = class(TIniFile)
  public
    constructor {%H-}Create;
  end;

const
  LangDirectory = 'localization';

var
  Lang : TLang;

function  T(const id : string): string;
function TT(const id : string): string;

implementation

constructor TLang.Create;
begin
  inherited Create(AppPath + Slash + LangDirectory + Slash + LowerCase(facelang) + '.lng');
end;

function T(const id : string): string;
begin
  Result := Lang.ReadString('Interface',id,id);
end;

function TT(const id : string): string;
var s : string;
begin
  s := Lang.ReadString('Interface',id,id) ;
  Result := AnsiReplaceStr(s,'.','');
end;

end.

