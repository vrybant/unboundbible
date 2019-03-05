unit UnitLang;

interface

uses
  SysUtils, Classes, Fgl, Graphics, IniFiles,  ClipBrd, LCLProc;

type
  TLocal = class
    filename : string;
    language : string;
    id : string;
  end;

  TLocalization = class(TFPGList<TLocal>)
  private
    function GetFileName(id: string): string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  Localization : TLocalization;
  LocalLang : string;

var
  ms_Commentary,
  ms_Confirm,
  ms_Strong,
  ms_File,
  ms_Footnote,
  ms_Found,
  ms_Language,
  ms_Message,
  ms_MoreInfo,
  ms_NoModules,
  ms_NoResults,
  ms_Overwrite,
  ms_Save : string;

function T(const id : string): string;
procedure TranslateAll;

implementation

uses
  FormMain, FormAbout, FormSearch, FormCompare, UnitShelf, FormCopy, FormTranslate,
  FormCommentary, FormDownload, UnitLib;

var
  IniFile : TIniFile;

function T(const id : string): string;
begin
  Result := IniFile.ReadString('Localization',id,id);
end;

procedure TranslateConstants;
begin
  ms_Commentary := T('Commentaries');
  ms_Confirm := T('Confirmation');
  ms_Strong := T('Strong''s Dictionary');
  ms_File := T('File');
  ms_Footnote := T('Footnote');
  ms_Found := T('verses found');
  ms_Language := T('Language');
  ms_MoreInfo := T('For more information, choose Menu > Help, then click «Module downloads».');
  ms_NoModules := T('You don''t have any commentary modules.');
  ms_NoResults := T('You search for % produced no results.');
  ms_Overwrite := T('OK to overwrite %s?');
  ms_Save := T('Save changes?');
  ms_Message := T('This search returned too many results.') + ' ' +
                T('Please narrow your search.');
end;

procedure TranslateAll;
var
  filename : string;
begin
  filename := Localization.GetFileName(LocalLang);
  if filename = '' then Exit;
  IniFile := TIniFile.Create(filename);

  MainForm      .Translate;
  SearchForm    .Translate;
  CompareForm   .Translate;
  AboutBox      .Translate;
  CopyForm      .Translate;
  TranslateForm .Translate;
  CommentaryForm.Translate;
  DownloadForm  .Translate;

  TranslateConstants;

  IniFile.Free;
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
      Item.filename := f;

      IniFile := TIniFile.Create(f);
      Item.language := IniFile.ReadString('Details','Language','--');
      Item.id := IniFile.ReadString('Details','LangID','--');
      IniFile.Free;

      Add(Item);
    end;

  Sort(Comparison);
end;

function TLocalization.GetFileName(id: string): string;
var
  Item : TLocal;
begin
  Result := '';
  for Item in Self do
    if Item.id = id then Result := Item.filename;
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

end.


