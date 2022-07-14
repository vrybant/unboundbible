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
    procedure FormCreate(Sender: TObject);
  private
    TempOptions : TCopyOptions;
    procedure RemoveFormat(var s: string);
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

uses UnitLib, UnitUtils, UnitLocal;

const
  cgAbbreviate  = 0;
  cgEnumerated  = 1;
  cgGuillemets  = 2;
  cgParentheses = 3;
  cgEnd         = 4;
  cgBreak       = 5;
  cgPlain       = 6;

{$R *.lfm}

procedure TCopyForm.FormCreate(Sender: TObject);
begin
  {$ifdef linux} CheckGroup.Items.Delete(cgPlainText); {$endif}
end;

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
  CheckGroup.Items[cgBreak]       := T('Line Break'      ); {$ifndef linux}
  CheckGroup.Items[cgPlain]       := T('Plain text'      ); {$endif}

  CheckBox.Caption := T('Set Default');
end;

procedure TCopyForm.FormActivate(Sender: TObject);
begin
  {$ifdef linux}
    ButtonCancel.Top := 255;
    CheckBox.Left := ButtonCancel.Left - 2;
    CheckBox.Top := 290;
  {$endif}

  TempOptions := CopyOptions;

  CheckGroup.Checked[cgAbbreviate]  := CopyOptions.cvAbbreviate;
  CheckGroup.Checked[cgEnumerated]  := CopyOptions.cvEnumerated;
  CheckGroup.Checked[cgGuillemets]  := CopyOptions.cvGuillemets;
  CheckGroup.Checked[cgParentheses] := CopyOptions.cvParentheses;
  CheckGroup.Checked[cgEnd]         := CopyOptions.cvEnd;
  CheckGroup.Checked[cgBreak]       := CopyOptions.cvBreak; {$ifndef linux}
  CheckGroup.Checked[cgPlain]       := CopyOptions.cvPlain; {$endif}

  CheckBox.Checked:= False;

  LoadText;
  ButtonCopy.SetFocus;
end;

procedure TCopyForm.RemoveFormat(var s: string);
begin
  s := RemoveTags(s.Replace('<br>','[br]')).Replace('[br]','<br>');
end;

procedure TCopyForm.LoadText;
var
  s : string;
begin
  s := Tools.Get_Verses;
  if CopyOptions.cvBreak then s += '<br>';
  if CopyOptions.cvPlain then RemoveFormat(s);
  Memo.LoadText(s);
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
  CopyOptions.cvAbbreviate  := CheckGroup.Checked[cgAbbreviate];
  CopyOptions.cvEnumerated  := CheckGroup.Checked[cgEnumerated];
  CopyOptions.cvGuillemets  := CheckGroup.Checked[cgGuillemets];
  CopyOptions.cvParentheses := CheckGroup.Checked[cgParentheses];
  CopyOptions.cvEnd         := CheckGroup.Checked[cgEnd];
  CopyOptions.cvBreak       := CheckGroup.Checked[cgBreak]; {$ifndef linux}
  CopyOptions.cvPlain       := CheckGroup.Checked[cgPlain]; {$endif}
  LoadText;
  Repaint;
end;

procedure TCopyForm.ButtonCopyClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TCopyForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if self.ModalResult <> mrOk then CopyOptions := TempOptions;
  if (self.ModalResult = mrOk) and (not CheckBox.Checked) then CopyOptions := TempOptions;
end;

end.
