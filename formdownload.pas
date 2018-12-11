unit FormDownload;

interface

uses
  Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, CheckLst,
  Grids, SysUtils;

type

  { TDownloadForm }

  TDownloadForm = class(TForm)
    ButtonOK: TButton;
    Grid: TStringGrid;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure GridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    procedure MakeListBox;
    procedure ShelfToListBox;
    procedure ListBoxToShelf;
  public
    procedure Translate;
  end;

var
  DownloadForm: TDownloadForm;

implementation

uses UnitShelf, UnitLang;

{$R *.lfm}

procedure TDownloadForm.Translate;
begin
  Caption := ' ' + T('Modules');
  ButtonOK.Caption := T('OK');
end;

procedure TDownloadForm.FormCreate(Sender: TObject);
begin
  MakeListBox;

  {$ifdef darwin}
  ButtonOK.Visible := False;
  Height := 182;
  {$endif}
end;

procedure TDownloadForm.FormShow(Sender: TObject);
begin
 // ShelfToListBox;
end;

procedure TDownloadForm.FormShowHint(Sender: TObject; HintInfo: PHintInfo);
begin
  HintInfo.HintMaxWidth := 100;
end;

procedure TDownloadForm.GridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
begin
  HintText := Grid.Cells[ACol, ARow];
end;

procedure TDownloadForm.GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Rect : TRect;
  cx, cy : integer;
begin
  Grid.ShowHint := false;
  Grid.MouseToCell(x,y,cx,cy);
  if (cx >= 0) and (cy >= 0) and (cx < Grid.ColCount) and (cy < Grid.RowCount) then
    begin
      Rect := Grid.CellRect(cx,cy);
      if Grid.Canvas.TextWidth(Grid.Cells[cx,cy]) > Rect.Right - Rect.Left then
        Grid.ShowHint := true;
    end;
end;

procedure TDownloadForm.MakeListBox;
var i,j: integer;
begin
  Grid.RowCount := Shelf.Count + 1;

  Grid.Cells[0,0] := 'Abbreviation';
  Grid.Cells[1,0] := 'Title';
  Grid.Cells[2,0] := 'Copyright';
  Grid.Cells[3,0] := 'Language';
  Grid.Cells[4,0] := 'Filename';

  Grid.ColWidths[0] := 80;
  Grid.ColWidths[1] := 303;
  Grid.ColWidths[2] := 150;
  Grid.ColWidths[3] := 80;
  Grid.ColWidths[4] := 150;

  for i:=0 to Shelf.Count-1 do
    begin
      j := i + 1;
      Grid.Cells[0,j] := Shelf[i].abbreviation;
      Grid.Cells[1,j] := Shelf[i].name;
      Grid.Cells[2,j] := Shelf[i].copyright;
      Grid.Cells[3,j] := Shelf[i].language;
      Grid.Cells[4,j] := Shelf[i].filename;

      Grid.Cells[1,j] := Shelf[i].name;
    end;
end;

procedure TDownloadForm.ShelfToListBox;
var i: integer;
begin
end;

procedure TDownloadForm.ListBoxToShelf;
begin
end;

procedure TDownloadForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ListBoxToShelf;
end;

end.

