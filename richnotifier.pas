unit RichNotifier;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, PopupNotifier, RichMemoEx;

type
  TNotifierForm = class({$ifdef windows} THintWindow {$else} TForm {$endif})
  private
    Title: TLabel;
    btnX: TNotifierXButton;
    procedure CloseForm(Sender: TObject);
    procedure HandleResize(Sender: TObject);
  public
    Memo: TRichMemoEx;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

  TRichNotifier = class(TComponent)
  private
    NotifierForm: TNotifierForm;
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    procedure SetOnClose(const Value: TCloseEvent);
    function  GetOnClose:TCloseEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show;
    procedure ShowAtPos(x: Integer; y: Integer);
    procedure LoadRichText(Source: string);
  published
    property Color: TColor  read GetColor write SetColor;
    property Title: string read GetTitle write SetTitle;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnClose: TCloseEvent  read GetOnClose write SetOnClose;
  end;

implementation

const
  INT_NOTIFIER_FORM_WIDTH  = 325;
  INT_NOTIFIER_FORM_HEIGHT = 220; // 110
  INT_NOTIFIER_SPACING = 5;
  INT_NOTIFIER_BUTTON_SIZE = 20;

{ TNotifierForm }

constructor TNotifierForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BorderStyle := bsNone;

  Width := INT_NOTIFIER_FORM_WIDTH;
  Height := INT_NOTIFIER_FORM_HEIGHT;

  Title := TLabel.Create(Self);
  Title.Parent := Self;
  Title.AutoSize := False;
  Title.Transparent := True;
  Title.Font.Style := [FsBold];
  Title.Caption := 'Caption';
  Title.ParentColor := True;

  Memo := TRichMemoEx.Create(Self);
  Memo.Parent := Self;
  Memo.AutoSize := False;
  Memo.WordWrap := True;
  Memo.ParentColor := True;
  Memo.ReadOnly := True;
  Memo.BorderStyle := bsNone;
  Memo.ScrollBars := ssAutoVertical;

  BtnX := TNotifierXButton.Create(Self);
  BtnX.Parent := Self;
  BtnX.Color :=  Color;
  btnX.OnClick := CloseForm;

  HandleResize(Self);

  Color := $DCFFFF; // Doesn't work on Gtk

  OnShow := HandleResize;
end;

destructor TNotifierForm.Destroy;
begin
  Title.Free;
  Memo.Free;
  BtnX.Free;
  inherited Destroy;
end;

procedure TNotifierForm.Paint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,width,height));
  {$ifdef linux} Memo.HideCursor; {$endif}
end;

procedure TNotifierForm.CloseForm(Sender: TObject);
var NoValue: TCloseAction;
begin
  if Assigned(OnClose) then OnClose(Self, NoValue);
  Close;
end;

procedure TNotifierForm.HandleResize(Sender: TObject);
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

{ TRichNotifier }

function TRichNotifier.GetTitle: string;
begin
  Result := NotifierForm.Title.Caption;
end;

procedure TRichNotifier.SetTitle(const Value: string);
begin
  NotifierForm.Title.Caption := Value;
end;

procedure TRichNotifier.SetOnClose(const Value: TCloseEvent);
begin
  NotifierForm.Onclose := Value;
end;

function TRichNotifier.GetOnClose:TCloseEvent;
begin
  Result := NotifierForm.Onclose;
end;


function TRichNotifier.GetVisible: Boolean;
begin
  Result := NotifierForm.Visible;
end;

procedure TRichNotifier.SetVisible(const Value: Boolean);
begin
  NotifierForm.Visible := Value;
end;

function TRichNotifier.GetColor: TColor;
begin
  Result := NotifierForm.Color;
end;

procedure TRichNotifier.SetColor(const Value: TColor);
begin
  NotifierForm.Color := Value;
end;

constructor TRichNotifier.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NotifierForm := TNotifierForm.Create(nil);
  NotifierForm.Visible := False;
end;

destructor TRichNotifier.Destroy;
begin
  NotifierForm.Close;
  NotifierForm.Free;
  inherited Destroy;
end;

procedure TRichNotifier.Show;
begin
  NotifierForm.ShowModal;
end;

procedure TRichNotifier.ShowAtPos(x: Integer; y: Integer);
begin
  if x + NotifierForm.Width > Screen.Width then
  begin
    NotifierForm.left := x - NotifierForm.Width;
    if NotifierForm.Left < 0 then NotifierForm.Left := 0;
  end
  else
    NotifierForm.left := x;

  if y + NotifierForm.Height > Screen.Height then
  begin
    NotifierForm.top := y - NotifierForm.Height;
    if NotifierForm.top < 0 then NotifierForm.top := 0;
  end
  else
    NotifierForm.top := y;

  Show;
end;

procedure TRichNotifier.LoadRichText(Source: string);
begin
  NotifierForm.Memo.LoadRichText(Source);
end;

end.
