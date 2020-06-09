unit FormDownload;

interface

uses
  Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, Grids, SysUtils,
  UnboundMemo, UmLib;

type

  { TDownloadForm }

  TDownloadForm = class(TForm)
    ButtonOK: TButton;
    LabelTest: TLabel;
    StringGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure StringGridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
  public
    procedure LoadGrid(Strings: TStringsArray);
    procedure Localize;
  end;

var
  DownloadForm: TDownloadForm;

implementation

uses UnitShelf, UnitLang;

{$R *.lfm}

procedure TDownloadForm.FormCreate(Sender: TObject);
begin
  StringGrid.RowCount := 1;
  Application.HintPause := 1;
end;

procedure TDownloadForm.StringGridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
const
  delta = {$ifdef windows} 4 {$else} 6 {$endif};
begin
  HintText := '';
  LabelTest.Visible := True;
  LabelTest.Caption := StringGrid.Cells[ACol, ARow];
  if LabelTest.Width > StringGrid.Columns[ACol].Width - delta then
    HintText := StringGrid.Cells[ACol, ARow];
  LabelTest.Visible := False;
end;

procedure TDownloadForm.LoadGrid(Strings: TStringsArray);
var
  List : TStringArray;
  index : integer = 1;
begin
   for List in Strings do
     begin
       StringGrid.InsertRowWithValues(index, List);
       index += 1;
     end;
   StringGrid.SortColRow(True, 1);
end;

procedure TDownloadForm.Localize;
begin
  Caption := ' ' + T('Modules');
  ButtonOK.Caption := T('OK');
end;

end.

