unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DelphiLog;

type
  TForm1 = class(TForm)
    btnWriteLog: TButton;
    btnLogEnabled: TButton;
    edtMessage: TEdit;
    procedure btnWriteLogClick(Sender: TObject);
    procedure btnLogEnabledClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnLogEnabledClick(Sender: TObject);
begin
  TDelphiLog.Enabled := not TDelphiLog.Enabled;
  if (TDelphiLog.Enabled) then
    btnLogEnabled.Caption := 'Disable Log'
  else
    btnLogEnabled.Caption := 'Enable Log';
end;

procedure TForm1.btnWriteLogClick(Sender: TObject);
begin
  TDelphiLog.WriteMessage(edtMessage.Text);
end;

end.
