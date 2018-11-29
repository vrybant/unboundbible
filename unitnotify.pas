unit UnitNotify;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  PopupNotifier, UnboundMemo;

type

  TNotifyForm = class(TForm)
    Title: TLabel;
    Memo: TUnboundMemo;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    btnX: TNotifierXButton;
    procedure HandleResize(Sender: TObject);
    procedure CloseForm(Sender: TObject);
  public
    procedure ShowAtPos(x: Integer; y: Integer);
  end;

var
  NotifyForm: TNotifyForm;

implementation

{$R *.lfm}

const
  INT_NOTIFIER_FORM_WIDTH  = 325;
  INT_NOTIFIER_FORM_HEIGHT = 155; // 110
  INT_NOTIFIER_SPACING = 5;
  INT_NOTIFIER_BUTTON_SIZE = 20;

{ TNotifyForm }

procedure TNotifyForm.FormCreate(Sender: TObject);
begin
  BorderStyle := bsNone;

  Width := INT_NOTIFIER_FORM_WIDTH;
  Height := INT_NOTIFIER_FORM_HEIGHT;

  Title.AutoSize := False;
  Title.Transparent := True;
  Title.Font.Style := [FsBold];
  Title.Caption := 'Caption';
  Title.ParentColor := True;

  Memo.AutoSize := False;
  Memo.WordWrap := True;
  Memo.ParentColor := True;
  Memo.ReadOnly := True;
  Memo.BorderStyle := bsNone;
  Memo.ScrollBars := ssAutoVertical;

  BtnX := TNotifierXButton.Create(Self);
  BtnX.Parent := Self;
  BtnX.Color :=  Color;
  BtnX.OnClick := CloseForm;

  HandleResize(Self);

  Color := $DCFFFF; // Doesn't work on Gtk

  OnShow := HandleResize;
end;

procedure TNotifyForm.FormActivate(Sender: TObject);
begin
  inherited;
  Memo.HideCursor;
end;

procedure TNotifyForm.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TNotifyForm.FormDestroy(Sender: TObject);
begin
  BtnX.Free;
  inherited Destroy;
end;

procedure TNotifyForm.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,width,height));
  {$ifdef linux} Memo.HideCursor; {$endif}
end;

procedure TNotifyForm.HandleResize(Sender: TObject);
begin
  if Title <> nil then
  begin
    Title.Left := INT_NOTIFIER_SPACING;
    Title.Top := INT_NOTIFIER_SPACING;
    Title.Width := Width - (Title.Left + INT_NOTIFIER_SPACING);
    Title.Height := 20;
  end;

  if Memo <> nil then
  begin
    Memo.Left := 20;
    Memo.Top := Title.Top + Title.Height + INT_NOTIFIER_SPACING;
    Memo.Width := Width - (Memo.Left + INT_NOTIFIER_SPACING);
    Memo.Height := Height - (Memo.Top + INT_NOTIFIER_SPACING);
  end;

  if BtnX <> nil then
  begin
    BtnX.Left := Width - (INT_NOTIFIER_BUTTON_SIZE + 5);
    BtnX.Top := INT_NOTIFIER_SPACING;
    BtnX.Width := INT_NOTIFIER_BUTTON_SIZE;
    BtnX.Height := INT_NOTIFIER_BUTTON_SIZE;
  end;
end;

procedure TNotifyForm.CloseForm(Sender: TObject);
var NoValue: TCloseAction;
begin
  if Assigned(OnClose) then OnClose(Self, NoValue);
  Close;
end;

procedure TNotifyForm.ShowAtPos(x: Integer; y: Integer);
begin
  if x + Width > Screen.Width then
  begin
    Left := x - Width;
    if Left < 0 then Left := 0;
  end
  else
    Left := x;

  if y + Height > Screen.Height then
  begin
    Top := y - Height;
    if Top < 0 then Top := 0;
  end
  else
    Top := y;

  Show;
end;

end.

