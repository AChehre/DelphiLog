unit DelphiLog;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  THost = record
    ClassName: string;
    WindowName: string;
  end;

type
  TDelphiLog = class
  private
    Host: THost;
    fEnabled: Boolean;
    fShowErrorMessages: Boolean;
    procedure SendData(const copyDataStruct: TCopyDataStruct);
  public
    procedure WriteMessage(sMessage: string);
    property Enabled: Boolean read fEnabled write fEnabled;
    // Show error messages on client.
    property ShowErrorMessages: Boolean read fShowErrorMessages write fShowErrorMessages;
    constructor Create;
  end;

implementation

{ TDelphiLog }

/// <summary>
/// Send data to DelphiLog application.
/// </summary>
/// <param name="copyDataStruct">The TCopyDataStruct is defined in the Winapi.Windows.pas
/// unit and wraps the tagCOPYDATASTRUCT structure that contains the data to be passed.</param>
constructor TDelphiLog.Create;
begin
  Self.Enabled := True;
  Self.ShowErrorMessages := True;
end;

procedure TDelphiLog.SendData(const copyDataStruct: TCopyDataStruct);
var
  receiverHandle: THandle;
  res: integer;
begin
  receiverHandle := FindWindow(PChar(Host.ClassName), PChar(Host.WindowName));
  if (receiverHandle = 0) then
  begin
    if (Self.ShowErrorMessages) then ShowMessage('Copy Data Receiver NOT found!');
    Exit;
  end;

  res := SendMessage(receiverHandle, WM_COPYDATA, integer(Application.Handle),
    integer(@copyDataStruct));
end;

/// <summary>
/// Write message to DelphiLog application.
/// </summary>
/// <param name="sMessage">The message to write.</param>
procedure TDelphiLog.WriteMessage(sMessage: string);
var
  copyDataStruct: TCopyDataStruct;
begin
  if not Self.Enabled then
    Exit;
  copyDataStruct.dwData := 0; // use it to identify the message contents
  copyDataStruct.cbData := (1 + Length(sMessage)) * SizeOf(Char);
  copyDataStruct.lpData := PChar(sMessage);
  Self.Host.ClassName := 'TfrmMain';
  Self.Host.WindowName := 'Delphi Log-Host';
  Self.SendData(copyDataStruct);
end;
end.
