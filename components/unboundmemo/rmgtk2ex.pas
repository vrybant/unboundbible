unit rmGtk2ex;

interface

uses
  Gtk2, Glib2, Gdk2;

procedure Gtk2Copy(Handle: THandle);
procedure Gtk2Cut(Handle: THandle);
procedure Gtk2Paste(Handle: THandle);
procedure Gtk2HideCursor(Handle: THandle);

implementation

function GetTextView(Handle: THandle): PGtkTextView;
var
  Widget : PGtkWidget;
  TextWidget : PGtkTextView;
  Container : PGtkContainer;
  Children : PGList;
begin
  Result := nil;
  Widget := {%H-}PGtkWidget(Handle);
  Container := PGtkContainer(widget);
  try Children := gtk_container_get_children(Container) except exit end;
  Children := gtk_container_get_children(Container);
  if Assigned(Children) then TextWidget := PGtkTextView(Children^.Data);
  if Assigned(TextWidget) then Result := TextWidget;
end;

function GetTextBuffer(Handle: THandle): PGtkTextBuffer;
begin
  Result := gtk_text_view_get_buffer(GetTextView(Handle));
end;

procedure Gtk2Copy(Handle: THandle);
var
  Clipboard: PGtkClipboard;
begin
  Clipboard := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  gtk_text_buffer_copy_clipboard(GetTextBuffer(Handle), Clipboard);
end;

procedure Gtk2Cut(Handle: THandle);
var
  Clipboard: PGtkClipboard;
begin
  Clipboard := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  gtk_text_buffer_cut_clipboard(GetTextBuffer(Handle), Clipboard, True);
end;

procedure Gtk2Paste(Handle: THandle);
var
  Clipboard: PGtkClipboard;
begin
  Clipboard := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  gtk_text_buffer_paste_clipboard(GetTextBuffer(Handle), Clipboard, NULL, True);
end;

procedure Gtk2HideCursor(Handle: THandle);
begin
  gtk_text_view_set_cursor_visible(GetTextView(Handle), False);
end;

end.

