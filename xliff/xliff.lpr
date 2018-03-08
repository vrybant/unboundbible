program xliff;

uses
  Interfaces, SysUtils, Classes, Graphics, IniFiles,  ClipBrd, UnitLib, LCLProc;

const
  LangPath = '..\localization\';

//function ChangeTargetLanguage(line: string): string;
//begin
//  Replace(line,'target-language="en"','target-language="ru"');
//  Result := line;
//end;

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
  Xliff, Outlist : TStringList;
  Source, Target : string;
  Line, Outfile : string;
  i : integer;
begin
  IniFile := TIniFile.Create(LangPath + Item);
  Xliff := TStringList.Create;
  Outlist := TStringList.Create;

  Xliff.LoadFromFile('en.xliff');

  for i:=0 to Xliff.Count-1 do
    begin
      Line := Xliff[i];
      if Pos('<target>',Line) > 0 then Continue;
      Outlist.Add(Line);
      Source := GetSource(Line);
      if Source = '' then Continue;
      Target := IniFile.ReadString('Localization', Source, Source);
      Target := GetTab(Line) + '<target>' + Target + '</target>';
      Outlist.Add(Target);
    end;

  ForceDirectories('out');
  Outfile := 'out\' + ChangeFileExt(item,'.xliff');
  Outlist.SaveToFile(Outfile);

  IniFile.Free;
  Outlist.Free;
  Xliff.Free;
end;

procedure Execute;
var
  List : TStringList;
  i : integer;
begin
  List := TStringList.Create;
  GetFileList(LangPath + '*.lng', List, True);
  for i:=0 to List.Count-1 do Localizate(List[i]);
  List.Free;
end;

begin
  Execute;
end.

