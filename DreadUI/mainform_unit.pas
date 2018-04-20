unit mainform_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList,
  ExtCtrls, PairSplitter, Menus, StdCtrls, Types,
  base_node, contract_node, method_node, fieldform_unit;

const
      EnterKey = 13;
      NewContractStr = 'New contract';
      NewMethodStr = 'NewMethod';

      AddNewFieldCaption = 'Add new field';

type

  { TDreadUIForm }

  TDreadUIForm = class(TForm)
    NewContractFieldAction: TAction;
    Action2: TAction;
    ContractEditPanel: TPanel;
    AddFieldMenuItem: TMenuItem;
    NodeNameEdit: TEdit;
    ContractValueList: TListView;
    NewMethodAction: TAction;
    ContractTree: TTreeView;
    NewContractMenuItem: TMenuItem;
    NewMethodMenuItem: TMenuItem;
    NewContractAction: TAction;
    OpenAction: TAction;
    NewAction: TAction;
    MainActions: TActionList;
    MainImages: TImageList;
    MainToolbar: TToolBar;
    MainSplitter: TPairSplitter;
    LeftSplitterSide: TPairSplitterSide;
    MethodPopup: TPopupMenu;
    ContractFieldPopup: TPopupMenu;
    RightSplitterSide: TPairSplitterSide;
    ContractPopup: TPopupMenu;
    NewFileButton: TToolButton;
    OpenFileButton: TToolButton;
    procedure NewContractFieldActionExecute(Sender: TObject);
    procedure NodeNameEditKeyPress(Sender: TObject; var Key: char);
    procedure ContractTreeChange(Sender: TObject; Node: TTreeNode);
    procedure ContractTreeContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: boolean);
    procedure FormActivate(Sender: TObject);
    procedure NewActionExecute(Sender: TObject);
    procedure NewContractActionExecute(Sender: TObject);
    procedure NewMethodActionExecute(Sender: TObject);
    procedure OpenActionExecute(Sender: TObject);
  private
    FContractNode: TTreeNode;
    FMethodNode: TTreeNode;
    FCurrentNode : TBaseNode;
  public

  end;

var
  GDreadUIForm: TDreadUIForm;

implementation

{$R *.lfm}

{ TDreadUIForm }

procedure TDreadUIForm.FormActivate(Sender: TObject);
begin
  FContractNode := ContractTree.Items[0];
  FMethodNode := ContractTree.Items[1];
  ContractEditPanel.Hide;
end;

procedure TDreadUIForm.ContractTreeContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: boolean);
var
  node: TTreeNode;
begin
  ContractTree.PopupMenu := nil;
  node := ContractTree.GetNodeAt(MousePos.x, MousePos.y);
  if (node <> nil) then
  begin
    node.Selected := True;

    if ((node = FContractNode) or (node.Parent = FContractNode)) then
    begin
      ContractTree.PopupMenu := ContractPopup;
      Handled := True;
      ContractPopup.PopUp;
    end
    else if ((node = FMethodNode) or (node.Parent = FMethodNode)) then
    begin
      ContractTree.PopupMenu := MethodPopup;
      Handled := True;
      MethodPopup.PopUp;
    end;
  end;
end;

procedure TDreadUIForm.ContractTreeChange(Sender: TObject; Node: TTreeNode);
begin
  if ((Node = FContractNode) or (Node = FMethodNode)) then
  begin
    ContractEditPanel.Hide;
  end else
  begin
    FCurrentNode := TBaseNode(Node.Data);
    NodeNameEdit.Text:= Node.Text;

    if FCurrentNode is TContractNode then
    begin
      ContractEditPanel.PopupMenu := ContractFieldPopup;
    end;

    ContractEditPanel.Show;
  end;
end;

procedure TDreadUIForm.NodeNameEditKeyPress(Sender: TObject; var Key: char);
begin
  if ord(key) = EnterKey then
  begin
    FCurrentNode.Caption := NodeNameEdit.Text;
  end;
end;

procedure TDreadUIForm.NewActionExecute(Sender: TObject);
begin
  ShowMessage('NEW FILE');
end;

procedure TDreadUIForm.NewContractActionExecute(Sender: TObject);
var node : TContractNode;
  treeNode: TTreeNode;
begin
  treeNode := ContractTree.Items.AddChild(FContractNode, NewContractStr);
  node := TContractNode.Create(treeNode);
  treeNode.Data := node;
end;

procedure TDreadUIForm.NewContractFieldActionExecute(Sender: TObject);
begin
  GFieldForm.Caption := AddNewFieldCaption;
  GFieldForm.ShowModal;
end;

procedure TDreadUIForm.NewMethodActionExecute(Sender: TObject);
var
  node: TMethodNode;
  treeNode: TTreeNode;
begin
  treeNode := ContractTree.Items.AddChild(FMethodNode, NewMethodStr);
  node := TMethodNode.Create(treeNode);
  treeNode.Data := node;
end;

procedure TDreadUIForm.OpenActionExecute(Sender: TObject);
begin
  ShowMessage('OPEN FILE');
end;

end.
