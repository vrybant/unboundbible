unit FormCopy;

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, UnboundMemo, UnitTools;

type

  { TCopyForm }

  TCopyForm = class(TForm)
    ButtonCancel: TButton;
    ButtonCopy: TButton;
    CheckBox: TCheckBox;
    CheckGroup: TCheckGroup;
    Memo: TUnboundMemo;
    procedure ButtonCopyClick(Sender: TObject);
    procedure CheckGroupItemClick(Sender: TObject; {%H-}Index: integer);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    TempOptions : TCopyOptions;
    procedure LoadText;
    procedure CopyToClipboard;
  public
    ParagraphStart : integer;
    ParagraphEnd : integer;
    procedure Localize;
  end;

var
  CopyForm: TCopyForm;

implementation

uses FormMain, UnitUtils, UnitLocal;

const
  cgAbbreviate  = 0;
  cgEnumerated  = 1;
  cgGuillemets  = 2;
  cgParentheses = 3;
  cgEnd         = 4;

{$R *.lfm}

procedure TCopyForm.Localize;
begin
  Caption := ' ' + T('Copy Verses');

  CheckGroup  .Caption := T('Options' );
  ButtonCopy  .Caption := T('Copy'    );
  ButtonCancel.Caption := T('Cancel'  );

  CheckGroup.Items[cgAbbreviate]  := T('Abbreviation'    );
  CheckGroup.Items[cgEnumerated]  := T('Enumerated'      );
  CheckGroup.Items[cgGuillemets]  := T('Guillemets'      );
  CheckGroup.Items[cgParentheses] := T('Parentheses'     );
  CheckGroup.Items[cgEnd]         := T('Link in the end' );

  CheckBox.Caption := T('Set Default');
end;

procedure TCopyForm.FormActivate(Sender: TObject);
begin
  Memo.Font.Assign(MainForm.Font);

  {$ifdef linux}
    ButtonCancel.Top := 255;
    CheckBox.Left := ButtonCancel.Left - 2;
    CheckBox.Top := 290;
  {$endif}

  TempOptions := Options;

  CheckGroup.Checked[cgAbbreviate]  := Options.cvAbbreviate;
  CheckGroup.Checked[cgEnumerated]  := Options.cvEnumerated;
  CheckGroup.Checked[cgGuillemets]  := Options.cvGuillemets;
  CheckGroup.Checked[cgParentheses] := Options.cvParentheses;
  CheckGroup.Checked[cgEnd]         := Options.cvEnd;

  CheckBox.Checked:= False;

  LoadText;
end;

procedure TCopyForm.LoadText;
begin
  Memo.LoadText(Tools.Get_Verses());
end;

procedure TCopyForm.CopyToClipboard;
begin
  Memo.SelectAll;
  Memo.CopyToClipboard;
  Memo.SelStart  := 0;
  Memo.SelLength := 0;
end;

procedure TCopyForm.CheckGroupItemClick(Sender: TObject; Index: integer);
begin
  Options.cvAbbreviate  := CheckGroup.Checked[cgAbbreviate];
  Options.cvEnumerated  := CheckGroup.Checked[cgEnumerated];
  Options.cvGuillemets  := CheckGroup.Checked[cgGuillemets];
  Options.cvParentheses := CheckGroup.Checked[cgParentheses];
  Options.cvEnd         := CheckGroup.Checked[cgEnd];
  LoadText;
  Repaint;
end;

procedure TCopyForm.ButtonCopyClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TCopyForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if self.ModalResult <> mrOk then Options := TempOptions;
  if (self.ModalResult = mrOk) and (not CheckBox.Checked) then Options := TempOptions;
end;

end.

