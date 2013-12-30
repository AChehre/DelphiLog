unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TWMCopyData = packed record
    Msg: Cardinal;
    From: HWND; // Handle of the Window that passed the data
    CopyDataStruct: PCopyDataStruct; // data passed
    Result: Longint; // Use it to send a value back to the "Sender"
  end;
  
type
  TfrmMain = class(TForm)
    mmoStatus: TMemo;
    btnClose: TButton;
    btnSave: TButton;
    dlgSave: TSaveDialog;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    procedure AddMessage(sMessage: string);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

/// <summary>
/// Show message on status memo.
/// </summary>
/// <param name="sMessage"> message for writing in status memo.</param>
procedure TfrmMain.AddMessage(sMessage: string);
var
  status: string;
//  row: Integer;
//  sdatetime: string;
begin
//  row := mmoStatus.Lines.Count + 1;
//  sdatetime := DateTimeToStr(Now);
//  status := Format('[%4d] - [%s] %s', [row, sdatetime, sMessage]);
  status := sMessage;
  mmoStatus.Lines.Append(status);
end;


procedure TfrmMain.WMCopyData(var Msg: TWMCopyData);
var
  sMessage: string;
begin
  sMessage := PChar(Msg.CopyDataStruct.lpData);
  AddMessage(sMessage);
  Msg.Result := 1;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
 Close;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    mmoStatus.Lines.SaveToFile(dlgSave.FileName);
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  mmoStatus.Lines.Clear;
end;

end.
