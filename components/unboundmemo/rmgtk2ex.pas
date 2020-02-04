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
  Widget, TextWidget: PGtkWidget;
  List: PGList;
begin
  Result := nil;
  Widget := {%H-}PGtkWidget(Handle);
  List := gtk_container_get_children(PGtkContainer(Widget));
  if not Assigned(List) then Exit;
  TextWidget := PGtkWidget(List^.Data);
  if not Assigned(TextWidget) then Exit;
  Result := PGtkTextView(TextWidget);
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

