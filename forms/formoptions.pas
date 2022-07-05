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
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure ButtonFontClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  public
    function ShowModalEx(const Font: TFont): integer;
    procedure Localize;
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

uses UnitUtils, UnitLocal, UnitLib;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  //
end;

function TOptionsForm.ShowModalEx(const Font: TFont): integer;
begin
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

