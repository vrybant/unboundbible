unit FormOptions;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

procedure TOptionsForm.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

