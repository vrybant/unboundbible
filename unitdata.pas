unit UnitData;

interface

uses
  Classes, Fgl, SysUtils, Graphics, FileUtil, IniFiles, Zipper, UnitLib;

const
  ApplicationName = 'Unbound Bible';
  ApplicationVersion = '5.3';
  BibleDirectory = 'bibles';
  LangDirectory = 'localization';
  Untitled = 'Untitled';
  RecentMax = 10;

var
  ApplicationUpdate : boolean = false;
  DefaultFont: TFont;
  Local : string;

type
  TFileFormat = (unbound, mysword, mybible);

  TSearchOption = (caseSensitive, wholeWords);
  TSearchOptions = set of TSearchOption;

type
  TRange = record
    from, till : integer;
  end;

type
  TVerse = record
    book, chapter, number, count : integer;
  end;

  TVerseArray = array of TVerse;

  TBook = class
  public
    title   : string;
    abbr    : string;
    number  : integer;
    id      : integer;
    sorting : integer;
  end;

  TBooks = TFPGList<TBook>;

  TContent = record
    verse : TVerse;
    text : string;
  end;

  TContentArray = array of TContent;

  TCopyOptions = record
    cvAbbreviate, cvEnumerated, cvGuillemets, cvParentheses, cvEnd : boolean;
  end;

var
  CurrVerse : TVerse;
  Options : TCopyOptions;

  TitlesArray : array [1..66] of string = (
    'Genesis','Exodus','Leviticus','Numbers','Deuteronomy','Joshua','Judges','Ruth','1 Samuel','2 Samuel',
    '1 Kings','2 Kings','1 Chronicles','2 Chronicles','Ezra','Nehemiah','Esther','Job','Psalms','Proverbs',
    'Ecclesiastes','Song of Songs','Isaiah','Jeremiah','Lamentations','Ezekiel','Daniel','Hosea','Joel',
    'Amos','Obadiah','Jonah','Micah','Nahum','Habakkuk','Zephaniah','Haggai','Zechariah','Malachi','Matthew',
    'Mark','Luke','John','Acts','Romans','1 Corinthians','2 Corinthians','Galatians','Ephesians','Philippians',
    'Colossians','1 Thessalonians','2 Thessalonians','1 Timothy','2 Timothy','Titus','Philemon','Hebrews',
    'James','1 Peter','2 Peter','1 John','2 John','3 John','Jude','Revelation'
    );

  AbbrevArray : array [1..66] of string = (
    'Gen.','Ex.','Lev.','Num.','Deut.','Josh.','Judg.','Ruth','1 Sam.','2 Sam.','1 Kin.','2 Kin.','1 Chr.',
    '2 Chr.','Ezra','Neh.','Esth.','Job','Ps.','Prov.','Eccl.','Song','Is.','Jer.','Lam.','Ezek.','Dan.',
    'Hos.','Joel','Amos','Obad.','Jon.','Mic.','Nah.','Hab.','Zeph.','Hag.','Zech.','Mal.','Matt.','Mark',
    'Luke','John','Acts','Rom.','1 Cor.','2 Cor.','Gal.','Eph.','Phil.','Col.','1 Thess.','2 Thess.','1 Tim.',
    '2 Tim.','Titus','Philem.','Heb.','James','1 Pet.','2 Pet.','1 John','2 John','3 John','Jude','Rev.'
    );

  BibleHubArray : array [1..66] of string = (
    'genesis','exodus','leviticus','numbers','deuteronomy','joshua','judges','ruth','1_samuel','2_samuel',
    '1_kings','2_kings','1_chronicles','2_chronicles','ezra','nehemiah','esther','job','psalms','proverbs',
    'ecclesiastes','songs','isaiah','jeremiah','lamentations','ezekiel','daniel','hosea','joel','amos',
    'obadiah','jonah','micah','nahum','habakkuk','zephaniah','haggai','zechariah','malachi','matthew',
    'mark','luke','john','acts','romans','1_corinthians','2_corinthians','galatians','ephesians','philippians',
    'colossians','1_thessalonians','2_thessalonians','1_timothy','2_timothy','titus','philemon','hebrews',
    'james','1_peter','2_peter','1_john','2_john','3_john','jude','revelation'
    );

const
  minVerse : TVerse = (book: 1; chapter : 1; number : 1; count : 1);
  AcuteChar = #$CC#$81;

function unbound2mybible(id: integer): integer;
function mybible2unbound(id: integer): integer;
function IsNewTestament(n: integer): boolean;
procedure CreateDataDirectory;
function ConfigFile: string;
function DataPath: string;
function GetDatabaseList: TStringArray;
function HomeURL: string;
function DownloadsURL: string;
function IssueURL: string;
function DonateURL: string;

implementation

const
  MaxBooks = 88;

var
  myBibleArray : array [1..MaxBooks] of integer = (
    010,020,030,040,050,060,070,080,090,100,110,120,130,140,150,160,190,220,230,240,
    250,260,290,300,310,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,
    480,490,500,510,520,530,540,550,560,570,580,590,600,610,620,630,640,650,660,670,
    680,690,700,710,720,730,000,000,000,000,000,000,000,000,000,000,165,468,170,180,
    462,464,466,467,270,280,315,320
    );

function unbound2mybible(id: integer): integer;
begin
  Result := id;
  if (id > 0) and (id <= Length(myBibleArray)) then Result := myBibleArray[id];
end;

function mybible2unbound(id: integer): integer;
var i : integer;
begin
  Result := id;
  if id = 0 then Exit;
  for i:=1 to Length(myBibleArray) do
    if id = myBibleArray[i] then Exit(i);
end;

function IsNewTestament(n: integer): boolean;
begin
  Result := (n >= 40) and (n < 77);
end;

function DataPath: string;
begin
  Result := GetUserDir + ApplicationName;
end;

procedure CreateDataDirectory;
begin
  if not DirectoryExists(DataPath) then ForceDirectories(DataPath);
end;

function GetDatabaseList: TStringArray;
const
  ext : array [1..4] of string = ('.unbound','.bbli','.mybible','.SQLite3');
var
  List : TStringArray;
  s, item : string;
  index : integer = 0;
begin
  List := GetFileList(DataPath, '*.*');
  SetLength(Result, Length(List));

  for item in List do
    for s in ext do
      if Suffix(s, item) then
        begin
          Result[index] := item;
          index += 1;
        end;

  SetLength(Result, index);
end;

procedure UnzipDefaultsFiles;
var
  UnZipper: TUnZipper;
  List : TStringArray;
  f : string;
begin
  if not DirectoryExists(DataPath) then ForceDirectories(DataPath);
  if not ApplicationUpdate and (Length(GetDatabaseList) > 0) then Exit;

  List := GetFileList(SharePath + BibleDirectory, '*.zip');

  UnZipper := TUnZipper.Create;
  UnZipper.UseUTF8 := True;
  UnZipper.OutputPath := DataPath;

  for f in List do
    try
      UnZipper.FileName := f;
      UnZipper.UnZipAllFiles;
    except
      //
    end;

  UnZipper.Free;
end;

procedure RemoveOldFiles;
var
  f, t : string;
const
  OldFiles : array [1..5] of string = (
    'kjv+.unbound','kjv.unbound','rst+.unbound','rstw.unbound','ubio.unbound');
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
  if (Local = 'ru_RU') or (Local = 'uk_UA') then Result := 'ru';
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

