unit FormCommentary;

{$mode delphi}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, Menus,
  IniFiles, UnboundMemo;

type

  { TCommentaryForm }

  TCommentaryForm = class(TForm)
    Memo: TUnboundMemo;
    miCopy: TMenuItem;
    PopupMenu: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
  private
    procedure SaveIniFile;
    procedure ReadIniFile;
  public
    procedure Translate;
  end;

var
  CommentaryForm: TCommentaryForm;

implementation

uses FormMain, UnitLang, UnitLib;

{$R *.lfm}

procedure TCommentaryForm.Translate;
begin
  Caption := ' ' + T('Commentaries');
end;

procedure TCommentaryForm.FormCreate(Sender: TObject);
begin
  ReadIniFile;
  PopupMenu.Images := MainForm.Images;
  miCopy.ImageIndex := 1;
end;

procedure TCommentaryForm.FormDestroy(Sender: TObject);
begin
  SaveIniFile;
end;

procedure TCommentaryForm.FormPaint(Sender: TObject);
begin
  {$ifdef linux} Memo.HideCursor; {$endif}
end;

procedure TCommentaryForm.miCopyClick(Sender: TObject);
begin
  Memo.CopyToClipboard;
end;

procedure TCommentaryForm.PopupMenuPopup(Sender: TObject);
begin
  miCopy.Enabled := Memo.SelLength > 0;
end;

procedure TCommentaryForm.SaveIniFile;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ConfigFile);

  if WindowState = wsNormal then
    begin
      IniFile.WriteInteger('Commentary', 'Left',   CommentaryForm.Left);
      IniFile.WriteInteger('Commentary', 'Top',    CommentaryForm.Top);
      IniFile.WriteInteger('Commentary', 'Width',  CommentaryForm.Width);
      IniFile.WriteInteger('Commentary', 'Height', CommentaryForm.Height);
    end;

  IniFile.Free;
end;

procedure TCommentaryForm.ReadIniFile;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ConfigFile);

  Height := IniFile.ReadInteger('Commentary', 'Height', Screen.Height - 300);
  Width := IniFile.ReadInteger('Commentary', 'Width', Screen.Width div 5);
  Left := IniFile.ReadInteger('Commentary', 'Left', Screen.Width - Width - 50);
  Top := IniFile.ReadInteger('Commentary', 'Top', 120);

  IniFile.Free;
end;

end.

