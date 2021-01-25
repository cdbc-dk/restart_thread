unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LMessages,
  LCLIntf,
  ComCtrls;

const
  UnitVersion = '0.24.01.2021';
  { internal messages for use between thread & app }
  LM_CREATE  = LM_USER+1;
  LM_LISTEN  = LM_USER+3;
  LM_ACCEPT  = LM_USER+5;
  LM_WORKING = LM_USER+7;
  LM_DONE    = LM_USER+11;
  LM_DESTROY = LM_USER+13;
  LM_LOADED  = LM_USER+17;
  LM_SENDING = LM_USER+19; { 07.04.2020 /bc }
  LM_SENT    = LM_USER+23; { 07.04.2020 /bc }
  LM_ERROR   = LM_USER+31; { 07.04.2020 /bc }
  { result codes from functions, more flexible than boolean }
  HR_OK = 0;
  HR_ERROR = -1;

type
  { * * * TTstThread * * * }
  TTstThread = class(TThread,IFPObserved)
  protected

    fHandle: THandle;
    fCounter: longint;
    procedure Execute; override;
  public
    constructor Create(const aHandle: THandle);
    destructor Destroy; override;
  end;
  { TForm1 }
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected

    procedure LMWorking(var Message: TLMessage); message LM_WORKING;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TTstThread }

procedure TTstThread.Execute;
var
  Idx: longint;
begin
  while not Terminated do try
    for Idx:= 0 to 999 do PostMessage(fHandle,LM_WORKING,Idx,999);
  except

  end;

end;

constructor TTstThread.Create(const aHandle: THandle);
begin
  inherited Create(true);  { create in suspended state, so we can set vars }
  FreeOnTerminate:= false;                     { we will free it ourselves }
  fHandle:= aHandle;     { a handle to our owner application, for messages }
  Start;                                  { start the whole circus running }
end;

destructor TTstThread.Destroy;
begin
  fHandle:= 0;
  inherited Destroy;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.LMWorking(var Message: TLMessage);
begin
  if Message.Msg:= LM_WORKING then begin
    ProgressBar1.Position:= Message.WParam;
    ProgressBar1.Max:= Message.LParam;
  end;
end;

end.

