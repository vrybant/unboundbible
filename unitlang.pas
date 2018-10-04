unit UnitLang;

interface

uses
  SysUtils, Classes, Graphics, IniFiles,  ClipBrd, UnitLib, LCLProc;

type
  TLanguage = class(TIniFile)
  public
    constructor {%H-}Create;
  end;

  TLocalization = class(TStringList)
  public
    constructor Create;
    function Native(index: integer): string;
  end;

var
  Localization : TLocalization;
  Language : TLanguage;

function  T(const id : string): string;

implementation

constructor TLanguage.Create;
begin
  inherited Create(SharePath + Slash + LangDirectory + Slash + LowerCase(facelang) + '.lng');
end;

function T(const id : string): string;
begin
  Result := Language.ReadString('Localization',id,id);
end;

//-------------------------------------------------------------------------------------------------

function NativeLanguage(s: string): string;
begin
  s := LowerCase(s);
  Result := OneUpCase(s);

  if s = 'russian'   then Result := 'Русский';
  if s = 'spanish'   then Result := 'Español';
  if s = 'italian'   then Result := 'Italiano';
  if s = 'finnish'   then Result := 'Suomi';
  if s = 'ukrainian' then Result := 'Українська ';
end;

//-------------------------------------------------------------------------------------------------
//                                       TLocalization
//-------------------------------------------------------------------------------------------------

function Comparison(List: TStringList; index1, index2: integer): integer;
begin
  Result := CompareText(NativeLanguage(List[index1]), NativeLanguage(List[index2]));
end;

constructor TLocalization.Create;
var
  List : TStringArray;
  f : string;
  i : integer;
begin
  inherited;

  List := GetFileList(SharePath + LangDirectory, '*.lng');
  for f in List do self.Add(ExtractOnlyName(f));

  Self.CustomSort(Comparison);
end;

function TLocalization.Native(index: integer): string;
begin
  Result := NativeLanguage(self[index]);
end;

initialization
  Localization := TLocalization.Create;

finalization
  Localization.Free;

end.


