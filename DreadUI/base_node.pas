unit base_node;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls;

type

{ TBaseNode }

 TBaseNode = class
  private
    FTreeNode : TTreeNode;
    function ReadCaption : String;
    procedure WriteCaption(value : String);
  public
    property Caption : String read ReadCaption write WriteCaption;
    constructor Create(ATreeNode : TTreeNode);
end;

implementation

{ TBaseNode }

constructor TBaseNode.Create(ATreeNode: TTreeNode);
begin
  FTreeNode := ATreeNode;
end;

function TBaseNode.ReadCaption : String;
begin
  Result := FTreeNode.Text;
end;

procedure TBaseNode.WriteCaption(value: String);
begin
  FTreeNode.Text := value;
end;

end.

