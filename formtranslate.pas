unit FormTranslate;

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, IniFiles, UnboundMemo;

type
  TTranslateForm = class(TForm)
    Memo: TUnboundMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure SaveIniFile;
    procedure ReadIniFile;
  public
    procedure Translate;
  end;

var
  TranslateForm: TTranslateForm;

implementation

uses UnitLang, UnitLib;

{$R *.lfm}

procedure TTranslateForm.Translate;
begin
  Caption := ' ' + T('Translation');
end;

procedure TTranslateForm.FormCreate(Sender: TObject);
begin
  ReadIniFile;
end;

procedure TTranslateForm.FormDestroy(Sender: TObject);
begin
  SaveIniFile;
end;

procedure TTranslateForm.SaveIniFile;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ConfigFile);

  if WindowState = wsNormal then
    begin
      IniFile.WriteInteger('Translation', 'Left',   TranslateForm.Left);
      IniFile.WriteInteger('Translation', 'Top',    TranslateForm.Top);
      IniFile.WriteInteger('Translation', 'Width',  TranslateForm.Width);
      IniFile.WriteInteger('Translation', 'Height', TranslateForm.Height);
    end;

  IniFile.Free;
end;

procedure TTranslateForm.ReadIniFile;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ConfigFile);

  Height := IniFile.ReadInteger('Translation', 'Height', Screen.Height - 400);
  Width := IniFile.ReadInteger('Translation', 'Width', 300);
  Left := IniFile.ReadInteger('Translation', 'Left', 500);
  Top := IniFile.ReadInteger('Translation', 'Top', 120);

  IniFile.Free;
end;

end.

