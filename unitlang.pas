unit UnitLang;

interface

uses
  SysUtils, Classes, Graphics, IniFiles,  ClipBrd, LCLProc;

type
  TLanguage = class(TIniFile)
  public
    constructor Create;
  end;

  TLocalization = class(TStringList)
  public
    constructor Create;
    function Native(index: integer): string;
  end;

var
  Localization : TLocalization;
  facelang : string;

var
  ms_Commentary,
  ms_Confirm,
  ms_Strong,
  ms_File,
  ms_Footnote,
  ms_Found,
  ms_Language,
  ms_Message,
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
  Language : TLanguage;

constructor TLanguage.Create;
begin
  inherited Create(SharePath + Slash + LangDirectory + Slash + LowerCase(facelang) + '.lng');
end;

function T(const id : string): string;
begin
  Result := Language.ReadString('Localization',id,id);
end;

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

procedure TranslateConstants;
begin
  ms_Commentary := T('Commentary');
  ms_Confirm := T('Confirmation');
  ms_Strong := T('Strong''s Dictionary');
  ms_File := T('File');
  ms_Footnote := T('Footnote');
  ms_Found := T('verses found');
  ms_Language := T('Language');
  ms_NoResults := T('You search for % produced no results.');
  ms_Overwrite := T('OK to overwrite %s?');
  ms_Save := T('Save changes?');
  ms_Message := T('This search returned too many results.') + ' ' +
                T('Please narrow your search.');
end;

procedure TranslateAll;
begin
  Language := TLanguage.Create;

  MainForm      .Translate;
  SearchForm    .Translate;
  CompareForm   .Translate;
  AboutBox      .Translate;
  CopyForm      .Translate;
  TranslateForm .Translate;
  CommentaryForm.Translate;
  DownloadForm  .Translate;

  TranslateConstants;

  Language.Free;
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
begin
  inherited;

  List := GetFileList(SharePath + LangDirectory, '*.lng');
  for f in List do self.Add(ExtractOnlyName(f));

  CustomSort(Comparison);
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


