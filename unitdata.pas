unit UnitData;

interface

uses
  Classes, SysUtils, Graphics, FileUtil, IniFiles, Zipper, UnitLib;

const
  ApplicationName = 'Unbound Bible';
  ApplicationVersion = '5.4';
  ModulesDirectory = 'modules';
  LangDirectory = 'localization';
  Untitled = 'Untitled';
  RecentMax = 10;

var
  ApplicationUpdate : boolean = false;
  DefaultFont: TFont;

const
  BibleHubArray : array [1..66] of string = (
    'genesis','exodus','leviticus','numbers','deuteronomy','joshua','judges','ruth','1_samuel','2_samuel',
    '1_kings','2_kings','1_chronicles','2_chronicles','ezra','nehemiah','esther','job','psalms','proverbs',
    'ecclesiastes','songs','isaiah','jeremiah','lamentations','ezekiel','daniel','hosea','joel','amos',
    'obadiah','jonah','micah','nahum','habakkuk','zephaniah','haggai','zechariah','malachi','matthew',
    'mark','luke','john','acts','romans','1_corinthians','2_corinthians','galatians','ephesians','philippians',
    'colossians','1_thessalonians','2_thessalonians','1_timothy','2_timothy','titus','philemon','hebrews',
    'james','1_peter','2_peter','1_john','2_john','3_john','jude','revelation'
    );

procedure CreateDataDirectory;
function ConfigFile: string;
function DataPath: string;
function GetDatabaseList: TStringArray;
function HomeURL: string;
function DownloadsURL: string;
function IssueURL: string;
function DonateURL: string;

implementation

uses UnitTools, UnitLocal;

function DataPath: string;
begin
  Result := GetUserDir + ApplicationName;
end;

procedure CreateDataDirectory;
begin
  if not DirectoryExists(DataPath) then ForceDirectories(DataPath);
end;

function GetDatabaseList: TStringArray;
var
  List : TStringArray;
  s, item : string;
const
  ext : array [1..4] of string = ('.unbound','.bbli','.mybible','.SQLite3');
begin
  List := GetFileList(DataPath, '*.*');
  Result := [];

  for item in List do
    for s in ext do
      if Suffix(s, item) then Result.Add(item);
end;

function GetUnboundBiblesList: TStringArray;
begin
  Result := GetFileList(DataPath, '*.bbl.unbound');
end;

procedure UnzipDefaultsFiles;
var
  UnZipper: TUnZipper;
  List : TStringArray;
  f, d : string;
  empty : boolean;
begin
  if not DirectoryExists(DataPath) then ForceDirectories(DataPath);

  empty := GetUnboundBiblesList.IsEmpty;
  if not ApplicationUpdate and not empty then Exit;

  List := GetFileList(SharePath + ModulesDirectory, '*.zip');

  UnZipper := TUnZipper.Create;
  UnZipper.UseUTF8 := True;
  UnZipper.OutputPath := DataPath;

  for f in List do
    begin
      d := DataPath + Slash + ExtractOnlyName(f);
      if not empty and not FileExists(d) then Continue;
      try
        UnZipper.FileName := f;
        UnZipper.UnZipAllFiles;
      except
        //
      end;
    end;

  UnZipper.Free;
end;

procedure RemoveOldFiles;
var
  f, t : string;
const
  OldFiles : array [1..5] of string = (
    'kjv+.unbound',
    'kjv.unbound',
    'rst+.unbound',
    'rstw.unbound',
    'ubio.unbound');
begin
  if not ApplicationUpdate then Exit;
  for f in OldFiles do
    begin
      t := DataPath + Slash + f;
      if FileExists(t) then DeleteFile(t);
    end;
end;

function ru: string;
begin
  Result := '';
  if (Localization.id = 'ru') or (Localization.id = 'uk') then Result := 'ru';
end;

function HomeURL: string;
begin
  Result := 'http://vladimirrybant.org/' + ru;
end;

function DownloadsURL: string;
begin
  Result := 'http://vladimirrybant.org/goto/ubdownload' + ru + '.php';
end;

function IssueURL: string;
begin
  Result := 'http://vladimirrybant.org/goto/contact' + ru + '.php'
end;

function DonateURL: string;
begin
  Result := 'http://vladimirrybant.org/goto/donate' + ru + '.php'
end;

function ConfigFile: string;
begin
  {$ifdef windows} Result := LocalAppDataPath + ApplicationName + Slash; {$endif}
  {$ifdef unix} Result := GetAppConfigDir(False); {$endif}
  Result += 'config.ini';
end;

procedure SaveConfig;
var IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ConfigFile);

  IniFile.WriteString('Application', 'Version', ApplicationVersion);
  IniFile.WriteString('Application', 'FontName', DefaultFont.Name);
  IniFile.WriteInteger('Application', 'FontSize', DefaultFont.Size);
  IniFile.WriteInteger('Verse', 'Book', CurrVerse.book);
  IniFile.WriteInteger('Verse', 'Chapter', CurrVerse.chapter);
  IniFile.WriteInteger('Verse', 'Number', CurrVerse.number);
  IniFile.WriteInteger('Verse', 'Count', CurrVerse.count);

  IniFile.Free;
end;

procedure ReadConfig;
var
  IniFile: TIniFile;
  Version: string;
const
  DefaultFontName = {$ifdef windows} 'Tahoma' {$else} 'default' {$endif};
  DefaultFontSize = 12;
begin
  IniFile := TIniFile.Create(ConfigFile);

  Version := IniFile.ReadString('Application', 'Version', '');
  ApplicationUpdate := ApplicationVersion <> Version;
  DefaultFont.Name := IniFile.ReadString('Application', 'FontName', DefaultFontName);
  DefaultFont.Size := IniFile.ReadInteger('Application', 'FontSize', DefaultFontSize);
  CurrVerse.book := IniFile.ReadInteger('Verse', 'Book', 0);
  CurrVerse.chapter := IniFile.ReadInteger('Verse', 'Chapter', 0);
  CurrVerse.number := IniFile.ReadInteger('Verse', 'Number', 0);
  CurrVerse.count := IniFile.ReadInteger('Verse', 'Count', 0);

  IniFile.Free;
end;

initialization
  DefaultFont := TFont.Create;
  ReadConfig;
  UnzipDefaultsFiles;
  RemoveOldFiles;

finalization
  SaveConfig;
  DefaultFont.Free;

end.

