unit FormOptions;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    ButtonFont: TButton;
    ButtonClose: TButton;
    ComboBoxLang: TComboBox;
    ComboBoxFont: TComboBox;
    Edit: TEdit;
    EditFont: TEdit;
    FontDialog: TFontDialog;
    LabelHistory: TLabel;
    LabelFont: TLabel;
    LabelLang: TLabel;
    procedure ComboBoxLangSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure ButtonFontClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    procedure MakeLangList;
    procedure MakeFontList;
  public
    procedure Localize;
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

uses FormMain, UnitUtils, UnitLocal, UnitLib;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  //
end;

procedure TOptionsForm.ComboBoxLangSelect(Sender: TObject);
begin
  Localization.SelectLanguage(ComboBoxLang.Items[ComboBoxLang.ItemIndex]);
  MainForm.LocalizeApplication;
end;

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  MakeLangList;
  MakeFontList;
  FontDialog.Font.Assign(Font);
end;

procedure TOptionsForm.FormPaint(Sender: TObject);
begin
  EditFont.Text := FontDialog.Font.Name + '; ' + FontDialog.Font.Size.ToString;
end;

procedure TOptionsForm.Localize;
begin
  Caption := ' ' + T('Options');
  LabelLang.Caption :=  T('Localization');
  LabelHistory.Caption := 'История';
  LabelFont.Caption := T('Font');
  ButtonClose.Caption := T('Close');
end;

procedure TOptionsForm.MakeLangList;
var
  Local : TLocal;
begin
  ComboBoxLang.Items.Clear;

  for Local in Localization do
    begin
      ComboBoxLang.Items.Add(Local.language);
      if Local.id = Localization.id then ComboBoxLang.ItemIndex := ComboBoxLang.Items.Count - 1;
    end;
end;

procedure TOptionsForm.MakeFontList;
var
  FontName: string;
begin
  ComboBoxFont.Items.Clear;
  for FontName in Screen.Fonts do
    begin
      ComboBoxFont.Items.Add(FontName);
      if FontName = FontDialog.Font.Name then ComboBoxFont.ItemIndex := ComboBoxFont.Items.Count - 1;
    end;
end;

procedure TOptionsForm.ButtonFontClick(Sender: TObject);
var
  TempFont : TFont;
begin
  TempFont := TFont.Create;
  TempFont.Assign(FontDialog.Font);

  if FontDialog.Execute then
    if FontDialog.Font.Name = '' then FontDialog.Font.Assign(TempFont);

  TempFont.Free;
end;

procedure TOptionsForm.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TOptionsForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

end.

