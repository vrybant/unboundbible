unit UnitNotify;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  PopupNotifier, UnboundMemo;

type

  { TNotifyForm }

  TNotifyForm = class(TForm)
    Title: TLabel;
    Memo: TUnboundMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    btnX: TNotifierXButton;
    procedure HandleResize;
    procedure CloseForm(Sender: TObject);
  public
    Compact: boolean;
    procedure ShowAtPos(Pos: TPoint; compact: boolean = true);
  end;

var
  NotifyForm: TNotifyForm;

implementation

{$R *.lfm}

procedure TNotifyForm.FormCreate(Sender: TObject);
begin
  Compact := true;

  BtnX := TNotifierXButton.Create(Self);
  BtnX.Parent := Self;
  BtnX.OnClick := CloseForm;

  Memo.BorderStyle := bsNone;
  Memo.AutoSize := False;
end;

procedure TNotifyForm.HandleResize;
const
  HEIGHT_MIN = 110;
  HEIGHT_MAX = 160;
  SPACING = 5;
  BUTTON_SIZE = 20;
begin
  if Compact then Height := HEIGHT_MAX
             else Height := HEIGHT_MIN;

  Title.Left := SPACING;
  Title.Top := SPACING;
  Title.Width := Width - Title.Left - SPACING;
  Title.Height := 20;

  Memo.Left := 20;
  Memo.Top := Title.Top + Title.Height + SPACING;
  Memo.Width := Width - Memo.Left - SPACING;
  Memo.Height := Height - Memo.Top - SPACING - SPACING;

  BtnX.Left := Width - BUTTON_SIZE - SPACING;
  BtnX.Top := SPACING;
  BtnX.Width := BUTTON_SIZE;
  BtnX.Height := BUTTON_SIZE;
end;

procedure TNotifyForm.FormShow(Sender: TObject);
begin
  HandleResize;
end;

procedure TNotifyForm.FormActivate(Sender: TObject);
begin
  Memo.HideCursor;
end;

procedure TNotifyForm.FormPaint(Sender: TObject);
begin
  {$ifdef linux} Memo.HideCursor; {$endif}
end;

procedure TNotifyForm.ShowAtPos(Pos: TPoint; compact: boolean = true);
begin
  Left := Pos.x;
  Top  := Pos.y;

  if Left + Width > Screen.Width then
    begin
      Left := Left - Width;
      if Left < 0 then Left := 0;
    end;

  if Top + Height > Screen.Height then
    begin
      Top := Top - Height;
      if Top < 0 then Top := 0;
    end;

  Show;
end;

procedure TNotifyForm.CloseForm(Sender: TObject);
begin
  Close;
end;

procedure TNotifyForm.FormDeactivate(Sender: TObject);
begin
  //Close;
end;

procedure TNotifyForm.FormDestroy(Sender: TObject);
begin
  BtnX.Free;
  inherited;
end;

end.

