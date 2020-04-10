unit FormDownload;

interface

uses
  Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, Grids,
  UnboundMemo, SysUtils;

type

  { TDownloadForm }

  TDownloadForm = class(TForm)
    ButtonOK: TButton;
    Memo: TUnboundMemo;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
  public
    procedure Localize;
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

procedure TDownloadForm.Localize;
begin
  Caption := ' ' + T('Modules');
  ButtonOK.Caption := T('OK');
end;

end.

