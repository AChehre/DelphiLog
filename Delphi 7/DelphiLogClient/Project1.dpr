program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CheDevFileLog in 'CheDevFileLog.pas',
  CheDevConsoleLog in 'CheDevConsoleLog.pas',
  CheDevSystem in 'CheDevSystem.pas',
  CheDevLog in 'CheDevLog.pas',
  CheDevLogManager in 'CheDevLogManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
