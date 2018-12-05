unit FormCommentary;

{$mode delphi}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, IniFiles, UnboundMemo;

type
  TCommentaryForm = class(TForm)
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
  CommentaryForm: TCommentaryForm;

implementation

uses UnitLang, UnitLib;

{$R *.lfm}

procedure TCommentaryForm.Translate;
begin
  Caption := ' ' + T('Commentary');
end;

procedure TCommentaryForm.FormCreate(Sender: TObject);
begin
  ReadIniFile;
end;

procedure TCommentaryForm.FormDestroy(Sender: TObject);
begin
  SaveIniFile;
end;

procedure TCommentaryForm.SaveIniFile;
var
  IniFile: TIniFile;
begin
  //IniFile := TIniFile.Create(ConfigFile);
  //
  //if WindowState = wsNormal then
  //  begin
  //    IniFile.WriteInteger('Commentary', 'Left',   TranslateForm.Left);
  //    IniFile.WriteInteger('Commentary', 'Top',    TranslateForm.Top);
  //    IniFile.WriteInteger('Commentary', 'Width',  TranslateForm.Width);
  //    IniFile.WriteInteger('Commentary', 'Height', TranslateForm.Height);
  //  end;
  //
  //IniFile.Free;
end;

procedure TCommentaryForm.ReadIniFile;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ConfigFile);

  Height := IniFile.ReadInteger('Commentary', 'Height', Screen.Height - 300);
  Width := IniFile.ReadInteger('Commentary', 'Width', Screen.Width div 5);
  Left := IniFile.ReadInteger('Commentary', 'Left', Screen.Width div 10 * 7);
  Top := IniFile.ReadInteger('Commentary', 'Top', 100);

  IniFile.Free;
end;

end.

