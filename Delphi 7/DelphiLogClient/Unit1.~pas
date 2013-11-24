unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DelphiLog;

type
  TForm1 = class(TForm)
    btnWriteLog: TButton;
    btnLogEnabled: TButton;
    edtMessage: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnWriteLogClick(Sender: TObject);
    procedure btnLogEnabledClick(Sender: TObject);
  private
    DelphiLog: TDelphiLog;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DelphiLog := TDelphiLog.Create;
end;

procedure TForm1.btnWriteLogClick(Sender: TObject);
begin
  DelphiLog.WriteMessage(edtMessage.Text);
end;

procedure TForm1.btnLogEnabledClick(Sender: TObject);
begin
  DelphiLog.Enabled := not DelphiLog.Enabled;
  if (DelphiLog.Enabled) then btnLogEnabled.Caption := 'Disable Log' else btnLogEnabled.Caption := 'Enable Log';
end;

end.
