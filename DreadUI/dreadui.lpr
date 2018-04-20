program dreadui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainform_unit, contract_node, base_node, method_node, fieldform_unit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDreadUIForm, GDreadUIForm);
  Application.CreateForm(TFieldForm, GFieldForm);
  Application.Run;
end.

