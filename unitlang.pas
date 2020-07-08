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

  TLocalizableStrings = record
    Commentary, Confirm, lsFile, Footnote, Found, Language, MoreInfo,
    Narrow, NoComMod, NoDicMod, NoComm, NoResults, NoXrefs, Overwrite, Save, Strong : string;
  end;

var
  Localization : TLocalization;
  ls : TLocalizableStrings;

function T(const id : string): string;
function GetDefaultLanguage: string;
procedure SetIniFile(filename: string);
procedure LocalizeStrings;

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

procedure LocalizeStrings;
begin
  ls.Commentary := T('Commentaries');
  ls.Confirm := T('Confirmation');
  ls.lsFile := T('File');
  ls.Footnote := T('Footnote');
  ls.Found := T('verses found');
  ls.Language := T('Language');
  ls.MoreInfo := T('For more information, choose Menu > Help, then click «Module downloads».');
  ls.NoComMod := T('You don''t have any commentary modules.');
  ls.NoDicMod := T('You don''t have any dictionary modules.');
  ls.NoResults := T('You search for % produced no results.');
  ls.NoComm := T('Commentaries not found.');
  ls.NoXrefs := T('Сross-references not found.');
  ls.Overwrite := T('OK to overwrite %s?');
  ls.Save := T('Save changes?');
  ls.Strong := T('Strong''s Dictionary');
  ls.Narrow := T('This search returned too many results.') + ' ' +
               T('Please narrow your search.');
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

