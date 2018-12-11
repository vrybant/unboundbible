unit FormDownload;

interface

uses
  Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, UnboundMemo, SysUtils;

type

  { TDownloadForm }

  TDownloadForm = class(TForm)
    ButtonOK: TButton;
    Memo: TUnboundMemo;
    procedure FormCreate(Sender: TObject);
  public
    procedure Translate;
  end;

var
  DownloadForm: TDownloadForm;

implementation

uses UnitShelf, UnitLang;

{$R *.lfm}

procedure TDownloadForm.FormCreate(Sender: TObject);
begin
  //
end;

procedure TDownloadForm.Translate;
begin
  Caption := ' ' + T('Modules');
  ButtonOK.Caption := T('OK');
end;

end.

