unit lfm_main;

{$mode objfpc}{$H+}
{$define UseObserver}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LMessages,
  LCLIntf,
  ComCtrls,
  RTLConsts;

const
  UnitVersion = '0.25.01.2021';
  { internal messages for use between thread & app }
  LM_CREATE     = LM_USER+1;
  LM_LISTEN     = LM_USER+3;
  LM_ACCEPT     = LM_USER+5;
  LM_WORKING    = LM_USER+7;
  LM_DONE       = LM_USER+11;
  LM_DESTROY    = LM_USER+13;
  LM_LOADED     = LM_USER+17;
  LM_SENDING    = LM_USER+19; { 07.04.2020 /bc }
  LM_SENT       = LM_USER+23; { 07.04.2020 /bc }
  LM_ERROR      = LM_USER+31; { 07.04.2020 /bc }
  LM_STATUS     = LM_USER+37; { 25.01.2021 /bc }
  LM_OBSCHANGED = LM_USER+41; { 27.01.2021 (bc }
  { result codes from functions, more flexible than boolean }
  HR_OK = 0;
  HR_ERROR = -1;

type
  { * * * TTstThread * * * }
  TTstThread = class(TThread,IFPObserved)
  protected
    fPause: boolean;
    fWaitEvent: PRtlEvent; // event for pausing the thread
    fWaitTimeout: ptruint; // how long to wait for an event before moving on
    fObservers: TFPList;
    fHandle: THandle;
    fCounter: longint;
    fObserver: TObject;
    procedure Execute; override;
  public
    constructor Create(const aHandle: THandle;anObserver: TObject;const aWaitTimeout: ptruint);
    destructor Destroy; override;
    // attach a new observer
    Procedure FPOAttachObserver(aObserver : TObject);
    // Detach an observer
    Procedure FPODetachObserver(aObserver : TObject);
    // Notify all observers of a change.
    Procedure FPONotifyObservers(aSender: TObject;aOperation: TFPObservedOperation;Data : Pointer);
    procedure Run;                            { get thread running (again) }
    procedure Pause;                           { pause the thread, waiting }
    procedure Stop;                        { break the loop and end thread }
  end;
  { TObserverRec }
  PObserverRec = ^TObserverRec;
  TObserverRec = packed record
    orSender: TObject;
    orOperaton: TFPObservedOperation;
    orData: pointer;
  end;

  { TForm1 }
  TForm1 = class(TForm,IFPObserver)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    fObsRec: TObserverRec;
    fThread: TTstThread;
    procedure LMObsChanged(var Message: TLMessage); message LM_OBSCHANGED;
    procedure LMWorking(var Message: TLMessage); message LM_WORKING;
    procedure LMStatus(var Message: TLMessage); message LM_STATUS;
  public
    // Called by observed when observers are notified.
    procedure FPOObservedChanged(aSender: TObject;Operation: TFPObservedOperation;Data: Pointer);
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
    if fPause then RtlEventWaitFor(fWaitEvent,fWaitTimeout);

    for Idx:= 0 to 999 do begin
      PostMessage(fHandle,LM_WORKING,Idx,999);
//      sleep(1);
    end;
    RTLeventResetEvent(fWaitEvent);
    {$ifdef UseObserver}
    FPONotifyObservers(Self,ooCustom,nil);
    {$endif}
  except
    // do nothing
  end;
  PostMessage(fHandle,LM_STATUS,Idx,longint(pchar('Thread Done!')));
end;

constructor TTstThread.Create(const aHandle: THandle;
                              anObserver: TObject;
                              const aWaitTimeout: ptruint);
begin
  inherited Create(true);  { create in suspended state, so we can set vars }
  FreeOnTerminate:= false;                     { we will free it ourselves }
  fHandle:= aHandle;     { a handle to our owner application, for messages }
  fObserver:= anObserver;                      { save for later detachment }
  {$ifdef UseObserver}
  FPOAttachObserver(fObserver);             { observer pattern, attach one }
  {$endif}
  fWaitTimeout:= aWaitTimeout;         { how long to wait before moving on }
  fWaitEvent:= RTLEventCreate;                    { initialize wait object }
  Start;                                  { start the whole circus running }
end;

destructor TTstThread.Destroy;
begin
  {$ifdef UseObserver}
  FPODetachObserver(fObserver);             { observer pattern, detach one }
  {$endif}
  RTLeventdestroy(fWaitEvent);                      { finalize wait object }
  fHandle:= 0;                       { disconnect the handle to owner form }
  inherited Destroy;
end;

{ implementation of the observer pattern }
procedure TTstThread.FPOAttachObserver(aObserver: TObject);
var
  I: IFPObserver;
begin
  if not aObserver.GetInterface(SGUIDObserver,I) then
    raise EObserver.CreateFmt(SErrNotObserver,[AObserver.ClassName]);
  if not assigned(fObservers) then fObservers:= TFPList.Create;
  fObservers.Add(I);
end;

procedure TTstThread.FPODetachObserver(aObserver: TObject);
var
  I: IFPObserver;
begin
  if not aObserver.GetInterface(SGUIDObserver,I) then
    raise EObserver.CreateFmt(SErrNotObserver,[AObserver.ClassName]);
  if Assigned(fObservers) then begin
    fObservers.Remove(I);
    if (fObservers.Count = 0) then FreeAndNil(fObservers);
  end;
end;

procedure TTstThread.FPONotifyObservers(aSender: TObject;aOperation: TFPObservedOperation;Data: Pointer);
var
  I: integer;
  Obs: IFPObserver;
begin
  if assigned(FObservers) then for I:= fObservers.Count-1 downto 0 do begin
    Obs:= IFPObserver(fObservers[i]);
    Obs.FPOObservedChanged(aSender,aOperation,Data);
  end;
end;

procedure TTstThread.Run;
begin
  RtlEventSetEvent(fWaitEvent);    { trigger wait object, execute will run }
  fPause:= false;
end;

procedure TTstThread.Pause;
begin
  fPause:= true;
//  RTLeventResetEvent(fWaitEvent);  { reset the event, so execute will wait }
end;

procedure TTstThread.Stop;
begin
  Terminate;     { will cause the thread to break out of the loop and stop }
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  fThread:= TTstThread.Create(Handle,Form1,60000); // 1 min

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  fThread.Run;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  fThread.Pause;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  fThread.Stop;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.LMObsChanged(var Message: TLMessage);
var S:string;
begin
  case fObsRec.orOperaton of
    ooAddItem:    S:= 'ooAddItem';
    ooChange:     S:= 'ooChange';
    ooDeleteItem: S:= 'ooDeleteItem';
    ooFree:       S:= 'ooFree';
    ooCustom:     S:= 'ooCustom';
  end;
  Memo1.Lines.Add('Observed changed: '+
                  fObsRec.orSender.ClassName+' '+
                  S+' '+
                  'Data');
  // TODO
end;

procedure TForm1.LMWorking(var Message: TLMessage);
begin
  if Message.Msg = LM_WORKING then begin
    ProgressBar1.Position:= Message.WParam;
    ProgressBar1.Max:= Message.LParam;
  end;
end;

procedure TForm1.LMStatus(var Message: TLMessage);
var
  S: string;
begin
  S:= string(pchar(Message.LParam));
  caption:= 'Status: '+S;
  FreeAndNil(fThread);
end;

{ Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer); }
procedure TForm1.FPOObservedChanged(aSender: TObject;
                                    Operation: TFPObservedOperation;
                                    Data: Pointer);


begin
  fObsRec.orSender:= aSender;                    { save data for use later }
  fObsRec.orOperaton:= Operation;
  fObsRec.orData:= Data;
  PostMessage(Handle,LM_OBSCHANGED,0,0);
end;

end.

