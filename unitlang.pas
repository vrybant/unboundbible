unit UnitLang;

interface

uses
  SysUtils, Classes, Fgl, Graphics, IniFiles, ClipBrd, LCLProc,
  UmLib, UnitLib, UnitData;

type
  TLocal = class
    language : string;
    id : string;
  end;

  TLocalization = class(TFPGList<TLocal>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  Localization : TLocalization;

function T(const id : string): string;
function GetDefaultLanguage: string;
procedure SetIniFile(filename: string);

implementation

var
  IniFile : TIniFile = nil;

function T(const id : string): string;
begin
  Result := IniFile.ReadString('Localization',id,id);
end;

function GetDefaultLanguage: string;
var
  List : TStringArray;
  f : string;
begin
  Result := 'en';
  List := GetFileList(SharePath + LangDirectory, '*.lng');

  for f in List do
    if ExtractOnlyName(f) = GetLanguageID then Result := GetLanguageID;
end;

procedure SetIniFile(filename: string);
begin
  if Assigned(IniFile) then IniFile.Free;
  IniFile := TIniFile.Create(filename);
end;

//-------------------------------------------------------------------------------------------------
//                                       TLocalization
//-------------------------------------------------------------------------------------------------

function Comparison(const Item1: TLocal; const Item2: TLocal): integer;
begin
  Result := CompareText(Item1.language, Item2.language);
end;

constructor TLocalization.Create;
var
  IniFile: TIniFile;
  Item : TLocal;
  List : TStringArray;
  f : string;
begin
  inherited;
  List := GetFileList(SharePath + LangDirectory, '*.lng');

  for f in List do
    begin
      Item := TLocal.Create;
      IniFile := TIniFile.Create(f);
      Item.language := IniFile.ReadString('Details','Language','--');
      Item.id := IniFile.ReadString('Details','LangID','--');
      IniFile.Free;
      Add(Item);
    end;

  Sort(Comparison);
end;

destructor TLocalization.Destroy;
var i : integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  inherited Destroy;
end;

initialization
  Localization := TLocalization.Create;

finalization
  Localization.Free;
  if Assigned(IniFile) then IniFile.Free;

end.

