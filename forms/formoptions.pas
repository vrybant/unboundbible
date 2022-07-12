unit FormOptions;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    ComboBoxMax: TComboBox;
    LabelHistory: TLabel;
    LabelFont: TLabel;
    LabelLang: TLabel;
    ComboBoxLang: TComboBox;
    ComboBoxFont: TComboBox;
    ComboBoxSize: TComboBox;
    ButtonClose: TButton;
    procedure ComboBoxFontSelect(Sender: TObject);
    procedure ComboBoxLangSelect(Sender: TObject);
    procedure ComboBoxMaxChange(Sender: TObject);
    procedure ComboBoxSizeSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    procedure MakeLangList;
    procedure MakeFontList;
    procedure MakeSizeList;
    procedure MakeMaxList;
  public
    procedure Localize;
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

uses FormMain, UnitTools, UnitUtils, UnitLocal, UnitLib;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  //
end;

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  MakeLangList;
  MakeFontList;
  MakeSizeList;
  MakeMaxList;
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

procedure TOptionsForm.ComboBoxMaxChange(Sender: TObject);
begin
  Tools.HistoryMax := ToInt(ComboBoxMax.Text);
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

procedure TOptionsForm.MakeMaxList;
var
  i : integer;
const
  arr : array [1..7] of integer = (10,20,50,100,200,500,1000);
begin
  ComboBoxMax.Text := Tools.HistoryMax.ToString;
  ComboBoxMax.Items.Clear;
  for i in arr do
    begin
      ComboBoxMax.Items.Add(ToStr(i));
      if i = Tools.HistoryMax then ComboBoxMax.ItemIndex := ComboBoxMax.Items.Count - 1;;
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

