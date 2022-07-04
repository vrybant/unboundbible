unit FormOptions;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    ButtonClose: TButton;
    ComboBox: TComboBox;
    Edit: TEdit;
    EditFont: TEdit;
    LabelHistory: TLabel;
    LabelFont: TLabel;
    LabelLang: TLabel;
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure Localize;
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

uses UnitUtils, UnitLocal;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  //
end;

procedure TOptionsForm.Localize;
begin
  Caption := ' ' + T('Options');
  LabelLang.Caption :=  T('Localization');
  LabelHistory.Caption := 'История';
  LabelFont.Caption := T('Font');
  ButtonClose.Caption := T('Close');
end;

procedure TOptionsForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

end.

