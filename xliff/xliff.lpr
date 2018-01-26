program xliff;

uses
  Interfaces, SysUtils, Classes, Graphics, IniFiles,  ClipBrd, UnitLib, LCLProc;

type
  TTarget = record
    value : string;
    from : integer;
    till : integer;
  end;

function GetTarget(s: string): TTarget;
begin
  Result.from := Pos('<target>',s);
  Result.till := Pos('</target>',s);

  if Result.from > 0 then
    begin
      Result.from := Result.from + Length('<target>');
      Result.value := Copy(s, Result.from, Result.till-Result.from);
    end;
end;

const
  LangPath = '..\localization\';

function ChangeString(s: string; Target: TTarget): string;
begin
  Result := Copy(s, 1, Target.from - 1) + Target.value + Copy(s, Target.till, 256);
end;

procedure Localizate(Item: string);
var
  XliffList : TStringList;
  IniFile: TIniFile;
  Target : TTarget;
  outfile : string;
  i : integer;
begin
  IniFile := TIniFile.Create(LangPath + Item);
  XliffList := TStringList.Create;
  XliffList.LoadFromFile('ru.xliff');

  for i:=0 to XliffList.Count-1 do
    begin
      Target := GetTarget(XliffList[i]);

      if Target.from > 0 then
        begin
          Target.value := IniFile.ReadString('Localization', Target.value, Target.value);
          XliffList[i] := ChangeString(XliffList[i], Target);
        end;

    end;

  ForceDirectories('out');
  outfile := 'out\' + ChangeFileExt(item,'.xliff');
  XliffList.SaveToFile(outfile);

  IniFile.Free;
  XliffList.Free;
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

