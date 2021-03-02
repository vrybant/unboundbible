unit UnitLocal;

interface

uses
  SysUtils, Classes, Fgl, Graphics, IniFiles, ClipBrd, LCLProc, UnitData, UnitLib;

type
  TLocalized = class
    filename : string;
    language : string;
    id : string;
  end;

  TLocalization = class(TFPGList<TLocalized>)
    LocalFile : TIniFile;
  public
    constructor Create;
    destructor Destroy; override;
    function DefaultLocal: string;
    procedure SetLocal(lang: string);
    function Translate(const s: string): string;
  end;

var
  Localization : TLocalization;

function T(const s: string): string;

implementation

function T(const s: string): string;
begin
  Result := Localization.Translate(s);
end;

//-------------------------------------------------------------------------------------------------
//                                       TLocalization
//-------------------------------------------------------------------------------------------------

function Comparison(const Item1: TLocalized; const Item2: TLocalized): integer;
begin
  Result := CompareText(Item1.language, Item2.language);
end;

constructor TLocalization.Create;
var
  IniFile: TIniFile;
  Item : TLocalized;
  List : TStringArray;
  f : string;
begin
  inherited;
  LocalFile := nil;
  List := GetFileList(SharePath + LangDirectory, '*.lng');

  for f in List do
    begin
      if Length(ExtractOnlyName(f)) = 2 then Continue; // remove old files
      Item := TLocalized.Create;
      IniFile := TIniFile.Create(f);
      Item.language := IniFile.ReadString('Details','Language','--');
      Item.id := IniFile.ReadString('Details','LangID','--');
      Item.filename := f;
      IniFile.Free;
      Add(Item);
    end;

  Sort(Comparison);
end;

function TLocalization.DefaultLocal: string;
var i : integer;
begin
  Result := 'en';

  for i:=0 to Count-1 do
    if Items[i].id = GetLanguageID then Result := Items[i].id;

  for i:=0 to Count-1 do
    if Items[i].id = GetLanguageIDs then Result := Items[i].id;
end;

procedure TLocalization.SetLocal(lang: string);
var
  filename : string;
  i : integer;
begin
  filename := SharePath + LangDirectory + Slash + 'english.lng';

  for i:=0 to Count-1 do
    if Items[i].id = lang then filename := Items[i].filename;

  if Assigned(LocalFile) then LocalFile.Free;
  LocalFile := TIniFile.Create(filename);
end;

function TLocalization.Translate(const s: string): string;
begin
  Result := LocalFile.ReadString('Localization',s,s);
end;

destructor TLocalization.Destroy;
var i : integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  if Assigned(LocalFile) then LocalFile.Free;
  inherited Destroy;
end;

initialization
  Localization := TLocalization.Create;

finalization
  Localization.Free;

end.

