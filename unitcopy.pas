unit UnitCopy;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, UnitLib, UnboundMemo, UnitStream;

type

  { TFormCopy }

  TFormCopy = class(TForm)
    ButtonCancel: TButton;
    ButtonCopy: TButton;
    CheckGroup: TCheckGroup;
    RadioGroup: TRadioGroup;
    Memo: TUnboundMemo;
    procedure ButtonCopyClick(Sender: TObject);
    procedure CheckGroupItemClick(Sender: TObject; {%H-}Index: integer);
    procedure FormActivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure RadioGroupClick(Sender: TObject);
  private
    procedure CopyToClipboard;
  public
    ParagraphStart : integer;
    ParagraphEnd : integer;
    procedure Translate;
  end;

var
  FormCopy: TFormCopy;

implementation

uses UnitTool, UnitType, UnitLang;

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
  Memo.SelectAll;
  Memo.CopyToClipboard;
  Memo.SelStart  := 0;
  Memo.SelLength := 0;
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
var
  Stream: TRichStream;
begin
  Stream := TRichStream.Create;
  Load_Verses(Stream);
  Memo.LoadRichText(Stream);
  Stream.Free;

  output('*');
end;

end.

