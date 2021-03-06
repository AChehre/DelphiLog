unit CheDevLogManager;

interface

uses CheDevLog, CheDevFileLog, CheDevConsoleLog;

type

  TCheDevLogManager = class
  private
    FCheDevLogFile: ICheDevLogFile;
    FCheDevLogConsole: ICheDevLogConsole;
    class function CheDevLogManager: TCheDevLogManager;
  public
    class procedure WriteMessage(sMessage: string);
  end;

implementation

{$WRITEABLECONST ON}
const _CheDevLogManager : TCheDevLogManager = nil;
{$WRITEABLECONST OFF}


{ TCheDevLogManager }

class function TCheDevLogManager.CheDevLogManager: TCheDevLogManager;
begin
  if _CheDevLogManager = nil then
  begin
    _CheDevLogManager :=TCheDevLogManager.Create;
    _CheDevLogManager.FCheDevLogFile := TCheDevLogFile.Create('C:\CheDevLog.txt');
    _CheDevLogManager.FCheDevLogConsole := TCheDevLogConsole.Create(False);
  end;
  Result := _CheDevLogManager;
end;

class procedure TCheDevLogManager.WriteMessage(sMessage: string);
begin
  TCheDevLogManager.CheDevLogManager.FCheDevLogFile.WriteMessage(sMessage);
  TCheDevLogManager.CheDevLogManager.FCheDevLogConsole.WriteMessage(sMessage);
end;

end.
 