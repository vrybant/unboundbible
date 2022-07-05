unit FormOptions;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    ButtonOK: TButton;
    ButtonFont: TButton;
    ButtonClose: TButton;
    ComboBox: TComboBox;
    Edit: TEdit;
    EditFont: TEdit;
    FontDialog: TFontDialog;
    LabelHistory: TLabel;
    LabelFont: TLabel;
    LabelLang: TLabel;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure ButtonFontClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    procedure MakeLangList;
  public
    function ShowModalEx(const Font: TFont): integer;
    procedure Localize;
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

uses UnitUtils, UnitLocal, UnitLib;

function TOptionsForm.ShowModalEx(const Font: TFont): integer;
begin
  MakeLangList;
  FontDialog.Font.Assign(Font);
  Result := ShowModal;
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
  ButtonOK.Caption := T('OK');
  ButtonClose.Caption := T('Close');
end;

procedure TOptionsForm.MakeLangList;
var
  Local : TLocal;
begin
  ComboBox.Items.Clear;

  for Local in Localization do
    begin
      ComboBox.Items.Add(Local.language);
      if Local.id = Localization.id then ComboBox.ItemIndex := ComboBox.Items.Count - 1;
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

