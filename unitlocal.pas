unit UnitLocal;

interface

uses
  SysUtils, Classes, Fgl, Graphics, IniFiles, ClipBrd, LCLProc, UnitUtils, UnitLib;

type
  TLocal = class
    filename : string;
    language : string;
    id : string;
  end;

  TLocalization = class(TFPGList<TLocal>)
  private
    FID : string;
    LocalFile : TIniFile;
    procedure SetID(Value: string);
    procedure SetLocalFile;
  public
    constructor Create;
    destructor Destroy; override;
    function DefaultID: string;
    function Translate(const s: string): string;
    property id: string read FID write SetID;
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

function Comparison(const Item1: TLocal; const Item2: TLocal): integer;
begin
  Result := CompareText(Item1.language, Item2.language);
end;

constructor TLocalization.Create;
var
  IniFile: TIniFile;
  Item : TLocal;
  f : string;
begin
  inherited;
  LocalFile := nil;

  for f in GetFileList(SharePath + LangDirectory, '*.lng') do
    begin
      if Length(ExtractOnlyName(f)) = 2 then Continue; // remove old files
      Item := TLocal.Create;
      IniFile := TIniFile.Create(f);
      Item.language := IniFile.ReadString('Details','Language','--');
      Item.id := IniFile.ReadString('Details','LangID','--');
      Item.filename := f;
      IniFile.Free;
      Add(Item);
    end;

  Sort(Comparison);
end;

function TLocalization.DefaultID: string;
var
  L : TLocal;
begin
  Result := 'en';
  for L in Self do if L.id = GetLanguageID  then Result := L.id;
  for L in Self do if L.id = GetLanguageIDs then Result := L.id;
end;

procedure TLocalization.SetLocalFile;
var
  Item : TLocal;
  filename : string;
begin
  filename := SharePath + LangDirectory + Slash + 'english.lng';

  for Item in Self do
    if Item.id = id then filename := Item.filename;

  if Assigned(LocalFile) then LocalFile.Free;
  LocalFile := TIniFile.Create(filename);
end;

procedure TLocalization.SetID(Value: string);
begin
  FID := Value;
  SetLocalFile;
end;

function TLocalization.Translate(const s: string): string;
begin
  Result := LocalFile.ReadString('Localization',s,s);
end;

destructor TLocalization.Destroy;
var
  Item : TLocal;
begin
  for Item in Self do Item.Free;
  if Assigned(LocalFile) then LocalFile.Free;
  inherited Destroy;
end;

end.

