unit FormNotify;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf,
  PopupNotifier, Menus, UnboundMemo;

type

  { TNotifyForm }

  TNotifyForm = class(TForm)
    miCopy: TMenuItem;
    PopupMenu: TPopupMenu;
    Title: TLabel;
    Memo: TUnboundMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MemoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miCopyClick(Sender: TObject);
  private
    btnX: TNotifierXButton;
    procedure HandleResize;
    procedure CloseForm(Sender: TObject);
    procedure ShowPopup;
  public
    Compact: boolean;
    procedure ShowAtPos(Pos: TPoint; compact: boolean = true);
  end;

var
  NotifyForm: TNotifyForm;

implementation

uses FormMain;

{$R *.lfm}

procedure TNotifyForm.FormCreate(Sender: TObject);
begin
  Compact := true;

  BtnX := TNotifierXButton.Create(Self);
  BtnX.Parent := Self;
  BtnX.OnClick := CloseForm;

  Memo.BorderStyle := bsNone;
  Memo.AutoSize := False;

  PopupMenu.Images := MainForm.Images;
  miCopy.ImageIndex := 1;
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

  Title.Left := 2 + SPACING;
  Title.Top  := 1 + SPACING;

  Memo.Left := 20;
  Memo.Top := Title.Top + Title.Height + SPACING;
  Memo.Width := Width - Memo.Left - SPACING;
  Memo.Height := Height - Memo.Top - SPACING - SPACING - 1;

  BtnX.Left := Width - BUTTON_SIZE - SPACING;
  BtnX.Top := SPACING;
  BtnX.Width := BUTTON_SIZE;
  BtnX.Height := BUTTON_SIZE;
end;

procedure TNotifyForm.FormActivate(Sender: TObject);
begin
  HandleResize;
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

procedure TNotifyForm.FormDestroy(Sender: TObject);
begin
  BtnX.Free;
  inherited;
end;

procedure TNotifyForm.MemoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then miCopy.Enabled := Memo.SelLength > 0;
end;

procedure TNotifyForm.miCopyClick(Sender: TObject);
begin
  Memo.CopyToClipboard;
end;

procedure TNotifyForm.ShowPopup;
var
  CursorPos: TPoint;
begin
  GetCursorPos(CursorPos);
  PopupMenu.Popup(CursorPos.X, CursorPos.Y);
end;


end.

