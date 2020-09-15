unit logformUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TLogForm }

  TLogForm = class(TForm)
    CloseButton: TButton;
    LogMemo: TMemo;
    Panel1: TPanel;
    procedure CloseButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  LogForm: TLogForm;
  logStr:AnsiString;

procedure ClearLog;
procedure AddLog(s:AnsiString);
procedure ShowLog;
procedure ShowLog(s:AnsiString);

implementation

{$R *.lfm}

{ TLogForm }

procedure ClearLog;
begin
   logStr := '';
end;

procedure AddLog(s:AnsiString);
begin
  if ( logStr <> '' ) then logStr := logStr + #13+#10;
  logStr := logStr + s;
end;

procedure ShowLog(s:AnsiString);
begin
  LogForm.LogMemo.Clear;
  if ( s = '' ) then begin
    LogForm.LogMemo.Lines.Add(logStr);
  end else begin
    LogForm.LogMemo.Lines.Add(s);
  end;
  LogForm.ShowModal;
end;

procedure ShowLog;
begin
  LogForm.LogMemo.Clear;
  LogForm.LogMemo.Lines.Add(logStr);
  LogForm.ShowModal;
end;

procedure TLogForm.CloseButtonClick(Sender: TObject);
begin
  Close
end;

end.

