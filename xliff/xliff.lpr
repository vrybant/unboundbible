program xliff;

uses
  Interfaces, SysUtils, Classes, Graphics, IniFiles,  ClipBrd, UnitLib, LCLProc;

const
  LangPath = '..\localization\';

procedure TargetLanguage(id: string; var line: string);
var
  source, target : string;
begin
  source := 'source-language="en"';
  target := 'target-language="&&"';
  if Pos(source, line) = 0 then Exit;
  if id = '' then Exit;
  Replace(target, '&&', id);
  Replace(line, source, source + ' ' + target);
end;

function GetSource(line: string): string;
var
  from, till, len : integer;
begin
  Result := '';
  from := Pos('<source>', line);
  till := Pos('</source>', line);
  len := Length('<source>');

  if from > 0 then
     Result := Copy(line, from+len, till-from-len);
end;

function GetTab(line: string): string;
var
  from : integer;
begin
  Result := '';
  from := Pos('<source>', line);
  if from > 0 then
     Result := Copy(line, 1, from-1);
end;

procedure Localizate(Item: string);
var
  IniFile: TIniFile;
  Xliff, List : TStringList;
  Source, Target : string;
  Line, Outfile, id : string;
  i : integer;
begin
  IniFile := TIniFile.Create(LangPath + Item);
  Xliff := TStringList.Create;
  List := TStringList.Create;
  id := IniFile.ReadString('Localization', 'LanguageID', '');

  Xliff.LoadFromFile('en.xliff');

  for i:=0 to Xliff.Count-1 do
    begin
      Line := Xliff[i];
      TargetLanguage(id, Line);
      if Pos('<target>', Line) > 0 then Continue;
      List.Add(Line);
      Source := GetSource(Line);
      if Source = '' then Continue;
      Target := IniFile.ReadString('Localization', Source, Source);
      Target := GetTab(Line) + '<target>' + Target + '</target>';
      List.Add(Target);
    end;

  ForceDirectories('out');
  Outfile := 'out\' + ChangeFileExt(item,'.xliff');
  List.SaveToFile(Outfile);

  IniFile.Free;
  List.Free;
  Xliff.Free;
end;

procedure Execute;
var
  List : TStringArray;
  item : string;
begin
  List := GetFileList(LangPath, '*.lng');
  for item in List do Localizate(ExtractFileName(item));
end;

begin
  Execute;
end.

