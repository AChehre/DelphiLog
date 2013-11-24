unit DelphiLog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  THost = record
    ClassName: string;
    WindowName: string;
  end;

type
  TDelphiLog = class
  private
    class var Host: THost;
    class var fEnabled: Boolean;
    class procedure SendData(const copyDataStruct: TCopyDataStruct);
  public
    class procedure WriteMessage(sMessage: string);
    class property Enabled: Boolean read fEnabled write fEnabled;
  end;

implementation

{ TDelphiLog }

/// <summary>
/// Send data to DelphiLog application.
/// </summary>
/// <param name="copyDataStruct">The TCopyDataStruct is defined in the Winapi.Windows.pas
/// unit and wraps the tagCOPYDATASTRUCT structure that contains the data to be passed.</param>
class procedure TDelphiLog.SendData(const copyDataStruct: TCopyDataStruct);
var
  receiverHandle: THandle;
  res: integer;
begin
  receiverHandle := FindWindow(PChar(Host.ClassName), PChar(Host.WindowName));
  if (receiverHandle = 0) then
  begin
    ShowMessage('CopyData Receiver NOT found!');
    Exit;
  end;

  res := SendMessage(receiverHandle, WM_COPYDATA, integer(Application.Handle),
    integer(@copyDataStruct));
end;

/// <summary>
/// Write message to DelphiLog application.
/// </summary>
/// <param name="sMessage">The message to write.</param>
class procedure TDelphiLog.WriteMessage(sMessage: string);
var
  copyDataStruct: TCopyDataStruct;
begin
  if not Self.Enabled then
    Exit;
  copyDataStruct.dwData := 0; // use it to identify the message contents
  copyDataStruct.cbData := (1 + Length(sMessage)) * SizeOf(Char);
  copyDataStruct.lpData := PChar(sMessage);
  Host.ClassName := 'TfrmMain';
  Host.WindowName := 'DelphiLog';
  SendData(copyDataStruct);
end;

initialization

TDelphiLog.Enabled := True;

end.
