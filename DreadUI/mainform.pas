unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList,
  ExtCtrls, PairSplitter, Menus;

type

  { TDreadUIForm }

  TDreadUIForm = class(TForm)
    ContractValueList: TListView;
    MenuItem1: TMenuItem;
    NewRequest: TAction;
    OpenAction: TAction;
    NewAction: TAction;
    MainActions: TActionList;
    MainImages: TImageList;
    MainToolbar: TToolBar;
    MainSplitter: TPairSplitter;
    LeftSplitterSide: TPairSplitterSide;
    RightSplitterSide: TPairSplitterSide;
    TreeMenu: TPopupMenu;
    NewFileButton: TToolButton;
    OpenFileButton: TToolButton;
    ContractTree: TTreeView;
    procedure NewActionExecute(Sender: TObject);
    procedure NewRequestExecute(Sender: TObject);
    procedure OpenActionExecute(Sender: TObject);
  private

  public

  end;

var
  DreadUIForm: TDreadUIForm;

implementation

{$R *.lfm}

{ TDreadUIForm }

procedure TDreadUIForm.NewActionExecute(Sender: TObject);
begin
  ShowMessage('NEW FILE');
end;

procedure TDreadUIForm.NewRequestExecute(Sender: TObject);
begin
  ShowMessage('NEW REQUEST');
end;

procedure TDreadUIForm.OpenActionExecute(Sender: TObject);
begin
  ShowMessage('OPEN FILE');
end;

end.

