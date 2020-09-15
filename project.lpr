program project;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormUnit, pascalscript, SpritePackerNodeUnit, logformUnit,
  SpritePackerUnit, AboutFormUnit, SettingsFormUnit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.

