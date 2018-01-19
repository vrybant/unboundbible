unit UnitSearch;

interface

uses
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, LCLType, UnitLib, UnitType;

type
  TSearchForm = class(TForm)
    OKButton: TButton;
    Edit: TEdit;
    RadioGroupRange: TRadioGroup;
    GroupBoxOption: TGroupBox;
    CheckBoxCase: TCheckBox;
    CheckBoxWhole: TCheckBox;
    procedure EditChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  public
    procedure Translate;
  end;

var
  SearchForm: TSearchForm;

function CurrentSearchOptions: TSearchOptions;
function CurrentSearchRange: TRange;

implementation

uses UnitShelf, UnitLang;

{$R *.lfm}

const
  rbEntireBible  = 0;
  rbOldTestament = 1;
  rbNewTestament = 2;
  rbGospels      = 3;
  rbEpistles     = 4;
  rbOpenedBook   = 5;

const
  rgEntireBible  : TRange = (from: -1; till: -1);
  rgOldTestament : TRange = (from:  1; till: 39);
  rgNewTestament : TRange = (from: 40; till: 66);
  rgGospels      : TRange = (from: 40; till: 43);
  rgEpistles     : TRange = (from: 45; till: 65);

function CurrentSearchOptions: TSearchOptions;
begin
  Result := [];
  if SearchForm.CheckBoxCase .Checked then Result := Result + [caseSensitive];
  if SearchForm.CheckBoxWhole.Checked then Result := Result + [wholeWords];
end;

function CurrentSearchRange: TRange;
begin
  Result := rgEntireBible;
  case SearchForm.RadioGroupRange.ItemIndex of
    rbOldTestament : Result := rgOldTestament;
    rbNewTestament : Result := rgNewTestament;
    rbGospels      : Result := rgGospels;
    rbEpistles     : Result := rgEpistles;
    rbOpenedBook   :
      begin
        Result.from := ActiveVerse.book;
        Result.till := ActiveVerse.book;
      end;
  end;
end;

procedure TSearchForm.Translate;
begin
  Caption := ' ' + T('Search');

  RadioGroupRange.Caption := T('Range'  ) + ' ';
  GroupBoxOption .Caption := T('Options') + ' ';

  RadioGroupRange.Items[0] := T('Entire Bible'  );
  RadioGroupRange.Items[1] := T('Old Testament'     );
  RadioGroupRange.Items[2] := T('New Testament'     );
  RadioGroupRange.Items[3] := T('Gospels' );
  RadioGroupRange.Items[4] := T('Epistles');
  RadioGroupRange.Items[5] := T('Opened Book' );

  CheckBoxWhole.Caption := T('Whole Words');
  CheckBoxCase .Caption := T('Case Sensitive' );

  OKButton     .Caption := T('Find');
end;

procedure TSearchForm.FormCreate(Sender: TObject);
begin
  OKButton.Enabled := False;
end;

procedure TSearchForm.EditChange(Sender: TObject);
begin
  OKButton.Enabled := Length(Trim(Edit.Text)) > 2;
end;

procedure TSearchForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) then Close;
end;

procedure TSearchForm.FormActivate(Sender: TObject);
begin
  if Bible.FileType = 'text' then
    begin
      RadioGroupRange.Enabled := False;
      RadioGroupRange.ItemIndex := rbEntireBible;
    end
  else
    RadioGroupRange.Enabled := True;
end;

end.

