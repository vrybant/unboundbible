unit UnitOptions;

interface

uses
  SysUtils, Classes, Graphics, Forms, Controls,
  StdCtrls, Buttons, ExtCtrls, Dialogs;

type

  { TFormOptions }

  TFormOptions = class(TForm)
    ButtonFont: TButton;
    ComboBoxTitles: TComboBox;
    FontDialog: TFontDialog;
    GroupBoxTitles: TGroupBox;
    GroupBoxFont: TGroupBox;
    LabelFont: TLabel;
    OKButton: TButton;
    procedure ButtonFontClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  public
    procedure Translate;
  end;

var
  FormOptions: TFormOptions;

implementation

uses UnitShelf, UnitLang, UnitLib;

{$R *.lfm}

procedure TFormOptions.Translate;
begin
  Caption := ' ' + T('Options.Caption');

  GroupBoxFont  .Caption := ' ' + T('Menu.Font') + ' ';
  GroupBoxTitles.Caption := ' ' + T('Options.TextOptions') + ' ';

  OKButton.Caption := T('Button.OK'    );
 end;

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  GetFileList(AppPath + Slash + TitleDirectory + Slash + '*.txt', ComboBoxTitles.Items, False);
  {$ifdef darwin}
  OKButton.Visible := False;
  Height := 175;
  {$endif}
end;

procedure TFormOptions.ButtonFontClick(Sender: TObject);
begin
  if FontDialog.Execute then CurrFont.Assign(FontDialog.Font);
  Repaint;
end;

procedure TFormOptions.FormActivate(Sender: TObject);
var i : integer;
begin
  FontDialog.Font.Assign(CurrFont);

  ComboBoxTitles.ItemIndex := 0;

  for i:=0 to ComboBoxTitles.Items.Count-1 do
    if LowerCase(ComboBoxTitles.Items[i]) = Bible.Language then
      ComboBoxTitles.ItemIndex := i;

  GroupBoxTitles.Enabled := not Bible.LangEnable;
  ComboBoxTitles.Enabled := not Bible.LangEnable;
end;

procedure TFormOptions.FormPaint(Sender: TObject);
begin
  LabelFont.Caption := FontDialog.Font.Name + '; ' + IntToStr(FontDialog.Font.Size);
end;

procedure TFormOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$ifdef unix} ModalResult := mrOk; {$endif}
  if ModalResult = mrOk then
    Bible.Language := LowerCase(ComboBoxTitles.Text);
end;

end.


