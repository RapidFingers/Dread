unit fieldform_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TFieldForm }

  TFieldForm = class(TForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    IsArrayCheck: TCheckBox;
    FieldNameEdit: TEdit;
    FieldTypeGroup: TRadioGroup;
    procedure CancelButtonClick(Sender: TObject);
  private

  public

  end;

var
  GFieldForm: TFieldForm;

implementation

{$R *.lfm}

{ TFieldForm }

procedure TFieldForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

end.

