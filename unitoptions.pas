unit UnitOptions;

interface

uses
  SysUtils, Classes, Graphics, Forms, Controls,
  StdCtrls, Buttons, ExtCtrls, Dialogs;

type
  TFormOptions = class(TForm)
    ButtonFont: TButton;
    FontDialog: TFontDialog;
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

uses UnitLang, UnitLib;

{$R *.lfm}

procedure TFormOptions.Translate;
begin
  Caption := ' ' + T('Options.Caption');
  GroupBoxFont.Caption := ' ' + T('Menu.Font') + ' ';
  OKButton.Caption := T('Button.OK'    );
 end;

procedure TFormOptions.FormCreate(Sender: TObject);
begin
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
begin
  FontDialog.Font.Assign(CurrFont);
end;

procedure TFormOptions.FormPaint(Sender: TObject);
begin
  LabelFont.Caption := FontDialog.Font.Name + '; ' + IntToStr(FontDialog.Font.Size);
end;

procedure TFormOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$ifdef unix} ModalResult := mrOk; {$endif}
end;

end.


