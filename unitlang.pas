unit UnitLang;

interface

uses
  SysUtils, Classes, Fgl, Graphics, IniFiles,  ClipBrd, LCLProc, UnitLib;

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
  InterfaceLang : string;

function T(const id : string): string;
procedure LocalizeApplication;
function GetDefaultLanguage: string;

implementation

uses
  FormMain, FormAbout, FormSearch, FormCompare, UnitShelf, FormCopy, FormCommentary,
  FormDownload, UnitData;

var
  IniFile : TIniFile;

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

function T(const id : string): string;
begin
  Result := IniFile.ReadString('Localization',id,id);
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

//-------------------------------------------------------------------------------------------------

procedure LocalizeApplication;
var
  filename : string;
begin
  filename := SharePath + Slash + LangDirectory + Slash + InterfaceLang + '.lng';
  IniFile := TIniFile.Create(filename);

  MainForm      .Localize;
  SearchForm    .Localize;
  CompareForm   .Localize;
  AboutBox      .Localize;
  CopyForm      .Localize;
  CommentaryForm.Localize;
  DownloadForm  .Localize;

  LocalizeStrings;

  IniFile.Free;
end;

initialization
  Localization := TLocalization.Create;

finalization
  Localization.Free;

end.


