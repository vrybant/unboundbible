unit UnitLib;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef mswindows} Windows, ShFolder, {$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, ClipBrd, FileUtil, LCLProc,
  {$ifdef unix} Process, {$endif} LazUtf8;

type
  TIntegerArray = array of integer;

  TRange = record
    from : integer;
    till : integer;
  end;

  TOptions = record
    cvAbbr  : boolean;
    cvEnd   : boolean;
    cvDelim : boolean;
    cvNum   : boolean;
    cvWrap  : boolean;
  end;

const
  AppName = 'Unbound Bible Tools';
  VersionInfo = '2.0';
  {$ifdef unix} RussianEdition = False; {$endif}

const
  BibleDirectory = 'bibles';
  TitleDirectory = 'titles';
  VerseDirectory = 'verse';

// string's functions

function IsNumeral(c: char): boolean;
function IsLetter(c: char): boolean;
function Marks(st: string): string;
function Prefix(ps, st: string): boolean;
function OneUpCase(st: string): string;
function MyStrToInt(st: string): integer;
function CleanString(s: string): string;
function StringSearch(subst: string; s: string): TIntegerArray;
procedure Replace(var s: string; const oldPattern, newPattern: String);
procedure StrToList(ch: Char; const st: string; List: TStringList);
procedure ListToStr(ch: Char; List: TStringList; var st: string);
function Utf8ToRTF(const s: string): string;

// stream's functions

procedure StreamWrite  (var Stream: TMemoryStream; s: string);
procedure StreamWriteLn(var Stream: TMemoryStream; s: string);
{$ifdef mswindows} procedure StreamToClipboard(Stream : TMemoryStream); {$endif}

// file's functions

function ExtractOnlyName(s: string): string;
procedure GetFileList(const Path: string; const List: TStrings; Ext: boolean);
function AppLocation: string;
function UserDocumentsPath : string;
function AppDataPath : string;
function IniFileName : string;
function TempFileName: string;
procedure CreateDirectories;
procedure OpenFolder(path : string);
{$ifdef darwin} procedure PrintFile(FileName : string); {$endif}

// system's functions

function GetDefaultLanguage: string;
procedure Output(s: string);

var
  CurrFont: TFont;
  Options : TOptions;
  facelang : string;

const
  Slash = DirectorySeparator;
  CRLF = #13 + #10;

implementation

// string's functions

function IsNumeral(c: char): boolean;
begin
  Result := c in ['0'..'9'];
end;

function IsLetter(c: char): boolean;
begin
  Result := ord(c) > 64;  // A-65
end;

function Marks(st: string): string;
begin
  Result := '"' + st + '"';
end;

function Prefix(ps, st: string): boolean;
begin
  Result := Pos(ps, st) = 1;
end;

function OneUpCase(st: string): string;
var
  st_up : string;
begin
  st_up := UpperCase(st);
  st[1] := st_up[1];
  Result := st;
end;

function MyStrToInt(st: string): integer;
var
  v : integer;
  r : integer;
begin
  st := Trim(st);
  Val(st, v, r);

  if r=0 then Result := v
         else Result := 0;
end;

function CleanString(s: string): string;
var i: integer;
begin
  for i:=1 to Length(s) do
    if not IsLetter(s[i]) then s[i] := ' ';
  Result := s;
end;

function StringSearch(subst: string; s: string): TIntegerArray;
var
  i,k,n : integer;
begin
  SetLength(Result,Length(s));

  k := 0;
  n := Pos(subst,s);

  while n > 0 do
    begin
      Inc(k);
      Result[k] := n;
      s := Copy(s, n + Length(subst), Length(s));
      n := Pos(subst,s);
    end;

  SetLength(Result,k);
end;

procedure Replace(var s: string; const oldPattern, newPattern: string);
begin
  s := StringReplace(s, oldPattern, newPattern, [rfReplaceAll]);
end;

procedure StrToList(ch: Char; const st: string; List: TStringList);
var
  p : array of integer;
  i : integer;
  n : integer;
  s : string;
begin
  List.Clear;

  SetLength(p,Length(st));
  p[1] := 0;
  n := 1;

  for i:=1 to Length(st) do
    if st[i] = ch then
      begin
        inc(n);
        p[n] := i;
      end;

  inc(n);
  p[n] := Length(st) + 1;

  for i:=1 to n-1 do
    begin
      s := copy(st,p[i]+1,p[i+1]-p[i]-1);
      s := Trim(s);
      List.Add(s);
    end;
end;

procedure ListToStr(ch: Char; List: TStringList; var st: string);
var i : integer;
begin
  st := '';

  for i:=0 to List.Count-1 do
    begin
      if st <> '' then st := st + ch;
      st := st + List[i];
    end;
end;

function ExtractOnlyName(s: string): string;
begin
  Result := ExtractFileName(ChangeFileExt(s,''));
end;

function Utf8ToRTF(const s: string): string;
var
  p: PChar;
  unicode: Cardinal;
  CharLen: integer;
begin
  Result := '';
  p := PChar(s);
  repeat
    unicode := UTF8CharacterToUnicode(p,CharLen);

    if unicode < $80 then Result := Result + char(unicode)
                     else Result := Result + '\u' + IntToStr(unicode) + '?';

    inc(p,CharLen);
  until (CharLen=0) or (unicode=0);
end;

// stream's functions

procedure StreamWrite(var Stream: TMemoryStream; s: string);
begin
  Stream.WriteBuffer(Pointer(s)^, Length(s));
end;

procedure StreamWriteLn(var Stream: TMemoryStream; s: string);
begin
  s := s + CRLF;
  Stream.WriteBuffer(Pointer(s)^, Length(s));
end;

{$ifdef mswindows}
procedure StreamToClipboard(Stream : TMemoryStream);
var
  Clipboard : TClipBoard;
     CF_RTF : Word;
begin
  Clipboard := TClipboard.Create ;
  CF_RTF  := RegisterClipboardFormat('Rich Text Format');
  Clipboard.AddFormat(CF_RTF,Stream);
  Clipboard.Free ;
end;
{$endif}

// file's functions

procedure GetFileList(const Path: string; const List: TStrings; Ext: boolean);
var
  SearchRec : TSearchRec;
  Res : integer;
  s : string;
begin
  Res  := SysUtils.FindFirst(Path, faAnyFile, SearchRec);

  while Res=0 do
    begin
      if Ext then s := SearchRec.Name
             else s := ExtractOnlyName(SearchRec.Name);

      if (SearchRec.Attr and faDirectory) = 0 then List.Add(s);
      Res := FindNext(SearchRec);
    end;

  SysUtils.FindClose(SearchRec);
end;

function AppLocation: string;
{$ifdef darwin} var n : integer; {$endif}
begin
  Result := Application.Location;
  {$ifdef darwin}
  n := Pos('MacOS',Result);
  if n > 0 then
    Result := Copy(Result,1,n-1) + 'Resources';
  {$endif}
end;

 (*
 CSIDL_PERSONAL, { My Documents }
 CSIDL_APPDATA, { Application Data }
 CSIDL_COMMON_APPDATA, { All Users\Application Data }
 CSIDL_WINDOWS, { GetWindowsDirectory() }
 CSIDL_SYSTEM,  { GetSystemDirectory() }
 CSIDL_PROGRAM_FILES, { C:\Program Files }
 CSIDL_MYPICTURES, { My Pictures }
 CSIDL_PROGRAM_FILES_COMMON, { C:\Program Files\Common }
 CSIDL_COMMON_DOCUMENTS, { All Users\Documents }
 *)

{$ifdef mswindows}
function GetSpecialFolderPath(FolderID: Cardinal): string;
var
  s : PChar;
begin
 Result := '';
 GetMem(s, Max_Path);
 try
   SHGetFolderPath(0, FolderID, 0, 0, s);
   Result := s;
 finally
   FreeMem(s, Max_Path);
 end;
end;
{$endif}

function UserDocumentsPath : string;
begin
{$ifdef mswindows}
  Result := GetSpecialFolderPath(CSIDL_PERSONAL);
{$else}
  Result := GetEnvironmentVariableUTF8('HOME');
{$endif}
end;

function AppDataFolder : string;
begin
{$ifdef mswindows}
  Result := GetSpecialFolderPath(CSIDL_APPDATA);
{$else}
  Result := GetEnvironmentVariableUTF8('HOME') + Slash + 'Library';
{$endif}
end;

function AppDataPath : string;
begin
  Result := AppDataFolder + Slash + AppName;
end;

function IniFileName: string;
begin
{$ifdef mswindows}
  Result := AppDataPath + Slash + 'config.ini';
{$else}
  Result := GetAppConfigFile(False);
{$endif}
end;

function TempFileName: string; // for printing
begin
  Result := AppDataPath + Slash + 'temp.rtf';
end;

procedure CreateDirectories;
var
  dir : string;
begin
  dir := AppDataPath + Slash + BibleDirectory;
  if not DirectoryExists(dir) then ForceDirectories(dir);

//  dir := AppDataPath + Slash + CommentaryDirectory;
//  if not DirectoryExists(dir) then ForceDirectories(dir);

  {$ifdef darwin}
  dir := ExtractFilePath(GetAppConfigFile(False));
  if not DirectoryExists(dir) then ForceDirectories(dir);
  {$endif}
end;

{$ifdef mswindows}
procedure OpenFolder(path : string);
begin
   ShellExecute(0,'open',PChar(marks(path)),'','',SW_SHOW);
end;
{$endif}

{$ifdef unix}
procedure OpenFolder(path : string);
begin
  with TProcess.Create(nil) do
  try
    CommandLine {%H-}:= 'open ' + marks(path);
    Options := [poUsePipes];
    try
      Execute;
    except
      on EProcess do ShowMessage('Oops! Looks like it can''t be opened.');
    end;
  finally
    Free;
  end;
end;
{$endif}

{$ifdef darwin}
procedure PrintFile(filename : string);
begin
  with TProcess.Create(nil) do
  try
    CommandLine {%H-}:=    'lp ' + marks(filename);
    Options := [poUsePipes]; // poWaitOnExit
    try
      Execute;
    except
      on EProcess do ShowMessage('Oops! Looks like it can''t be printed.');
    end;
  finally
    Free;
  end;
end;
{$endif}

 // system's functions

function GetDefaultLanguage: string;
begin
  Result := 'english';

  {$ifdef mswindows}
  case Lo(GetSystemDefaultLangID) of
    LANG_RUSSIAN   : Result := 'russian';
    LANG_SPANISH   : Result := 'spanish';
    LANG_ITALIAN   : Result := 'italian';
    LANG_FINNISH   : Result := 'finnish';
//  LANG_POLISH    : Result := 'polish';
//  LANG_FRENCH    : Result := 'french';
//  LANG_GERMAN    : Result := 'german';
//  LANG_UKRAINIAN : Result := 'ukrainian';
  end;
  {$endif}

  {$ifdef unix}
  if RussianEdition then {%H-}Result := 'russian';
  {$endif}
end;

procedure Output(s: string);
begin
  {$ifdef mswindows} OutputDebugString(PChar(s)) {$endif}
end;

initialization
  CurrFont := TFont.Create;
  CurrFont.Name := {$ifdef mswindows} 'Tahoma' {$else} 'default' {$endif};
  CurrFont.Size := {$ifdef mswindows} 12 {$else} 14 {$endif};

finalization
  CurrFont.Free;

end.

