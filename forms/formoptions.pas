unit FormOptions;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    EditMax: TEdit;
    LabelHistory: TLabel;
    LabelFont: TLabel;
    LabelLang: TLabel;
    ComboBoxLang: TComboBox;
    ComboBoxFont: TComboBox;
    ComboBoxSize: TComboBox;
    ButtonClose: TButton;
    procedure ComboBoxFontSelect(Sender: TObject);
    procedure ComboBoxLangSelect(Sender: TObject);
    procedure ComboBoxSizeSelect(Sender: TObject);
    procedure EditMaxChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    procedure MakeLangList;
    procedure MakeFontList;
    procedure MakeSizeList;
  public
    procedure Localize;
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

uses FormMain, UnitTools, UnitUtils, UnitLocal, UnitLib;

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  MakeLangList;
  MakeFontList;
  MakeSizeList;
  EditMax.Text := Tools.HistoryMax.ToString;
end;

procedure TOptionsForm.Localize;
begin
  Caption := ' ' + T('Options');
  LabelLang.Caption :=  T('Localization');
  LabelHistory.Caption := 'История';
  LabelFont.Caption := T('Font');
  LabelHistory.Caption := T('Maximum verses in history');
  ButtonClose.Caption := T('Close');
end;

procedure TOptionsForm.ComboBoxLangSelect(Sender: TObject);
begin
  Localization.SelectLanguage(ComboBoxLang.Items[ComboBoxLang.ItemIndex]);
  MainForm.LocalizeApplication;
end;

procedure TOptionsForm.ComboBoxFontSelect(Sender: TObject);
begin
  MainForm.Font.Name := ComboBoxFont.Items[ComboBoxFont.ItemIndex];
  MainForm.AssignFont;
end;

procedure TOptionsForm.ComboBoxSizeSelect(Sender: TObject);
begin
  MainForm.Font.Size := ComboBoxSize.Items[ComboBoxSize.ItemIndex].ToInteger;
  MainForm.AssignFont;
end;

procedure TOptionsForm.EditMaxChange(Sender: TObject);
const
  maximum = 1000;
begin
  Tools.HistoryMax := ToInt(EditMax.Text);
  if Tools.HistoryMax > maximum then Tools.HistoryMax := maximum;
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
  {$ifdef linux}
    ComboBoxFont.Items.Add('default');
    ComboBoxFont.ItemIndex := 0;
  {$endif}
  for FontName in Screen.Fonts do
    begin
      ComboBoxFont.Items.Add(FontName);
      if FontName = MainForm.Font.Name then ComboBoxFont.ItemIndex := ComboBoxFont.Items.Count - 1;
    end;
end;

procedure TOptionsForm.MakeSizeList;
var
  i : integer;
const
  arr : array [1..14] of integer = (8,9,10,11,12,13,14,16,18,20,22,24,28,32);
begin
  ComboBoxSize.Items.Clear;
  for i in arr do
    begin
      ComboBoxSize.Items.Add(ToStr(i));
      if i = MainForm.Font.Size then ComboBoxSize.ItemIndex := ComboBoxSize.Items.Count - 1;;
    end;
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

