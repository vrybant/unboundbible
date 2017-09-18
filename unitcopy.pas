unit UnitCopy;

{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, RichMemo, UnitLib, UnboundMemo;

type
  TFormCopy = class(TForm)
    ButtonCancel: TButton;
    ButtonCopy: TButton;
    CheckGroup: TCheckGroup;
    RadioGroup: TRadioGroup;
    RichMemoTemp: TRichMemo;
    procedure ButtonCopyClick(Sender: TObject);
    procedure CheckGroupItemClick(Sender: TObject; {%H-}Index: integer);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure RadioGroupClick(Sender: TObject);
  private
    Range: TRange;
    RichEditPreview : TUnboundMemo;
    Stream: TMemoryStream;
    procedure CopyToClipboard;
  public
    procedure Translate;
    procedure SetRange(r: TRange);
  end;

var
  FormCopy: TFormCopy;

implementation

uses UnitShelf, UnitTool, UnitType, UnitLang;

{$R *.lfm}

procedure TFormCopy.Translate;
begin
  Caption := ' ' + T('Verses.Caption');

  CheckGroup  .Caption := T('Verses.Options'  );
  RadioGroup  .Caption := T('Verses.Format'   );
  ButtonCopy  .Caption := T('Menu.Copy'       );
  ButtonCancel.Caption := T('Button.Cancel'   );

  CheckGroup .Items[0] := T('Verses.Abbr'     );
  CheckGroup .Items[1] := T('Verses.Brackets' );
  CheckGroup .Items[2] := T('Verses.End'      );
end;

procedure TFormCopy.SetRange(r: TRange);
begin
  Range := r;
end;

procedure TFormCopy.FormCreate(Sender: TObject);
begin
  Stream := TMemoryStream.Create;
  RichEditPreview := TUnboundMemo.Create(self);

  with RichEditPreview do
    begin
      Parent := FormCopy;
      HideSelection := True;
      ScrollBars := ssVertical;
      Hyperlink := False;
      Color := clWhite;
      Left := RichMemoTemp.Left;
      Top  := RichMemoTemp.Top;
      Width  := RichMemoTemp.Width;
      Height := RichMemoTemp.Height;
    end;

end;

procedure TFormCopy.FormActivate(Sender: TObject);
begin
  with Options do
    begin
      CheckGroup.Checked[0] := cvAbbr;
      CheckGroup.Checked[1] := cvDelim;
      CheckGroup.Checked[2] := cvEnd;

      if not cvWrap and not cvNum then RadioGroup.ItemIndex := 0;
      if     cvWrap and not cvNum then RadioGroup.ItemIndex := 1;
      if not cvWrap and     cvNum then RadioGroup.ItemIndex := 2;
      if     cvWrap and     cvNum then RadioGroup.ItemIndex := 3;
    end;
end;

procedure TFormCopy.CopyToClipboard;
begin
  RichEditPreview.SelectAll;
  RichEditPreview.CopyToClipboard;
  RichEditPreview.SetSel(0,0);
end;

procedure TFormCopy.CheckGroupItemClick(Sender: TObject; Index: integer);
begin
  Options.cvAbbr  := CheckGroup.Checked[0];
  Options.cvDelim := CheckGroup.Checked[1];
  Options.cvEnd   := CheckGroup.Checked[2];
  Repaint;
end;

procedure TFormCopy.ButtonCopyClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TFormCopy.RadioGroupClick(Sender: TObject);
begin
  Options.cvWrap := (RadioGroup.ItemIndex = 1) or (RadioGroup.ItemIndex = 3);
  Options.cvNum  := (RadioGroup.ItemIndex = 2) or (RadioGroup.ItemIndex = 3);
  Repaint;
end;

procedure TFormCopy.FormPaint(Sender: TObject);
begin
  Stream.Clear;
  ActiveVerse.Number := Range.from;
  ActiveVerse.Count  := Range.till;
  Load_Verses(Stream);
  RichEditPreview.LoadRichText(Stream);
  RichEditPreview.SetSel(0,0);
end;

procedure TFormCopy.FormDestroy(Sender: TObject);
begin
  Stream.Free;
  RichEditPreview.Free;
end;

end.

