unit SSPictureViewScroll;

interface

Uses
  {$IFDEF FPC}
  LCLType, LCLIntf, LMessages, Types,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Forms, Controls, Graphics;

type

  TWMSize           = TLMSize;
  TWMEraseBkgnd     = TLMEraseBkgnd;
  TMessage          = TLMessage;
  TSize             = Types.TSize;
  TWMHScroll        = TLMHScroll;
  TWMVScroll        = TLMVScroll;
  TWMKeyDown        = TLMKeyDown;
  TWMGetDlgCode     = TLMNoParams;

const
  WM_SIZE           = LM_SIZE;
  WM_ERASEBKGND     = LM_ERASEBKGND;
  WM_HSCROLL        = LM_HSCROLL;
  WM_VSCROLL        = LM_VSCROLL;
  WM_KEYDOWN        = LM_KEYDOWN;
  WM_GETDLGCODE     = LM_GETDLGCODE;
  CM_MOUSELEAVE     = $B000+20;

type
  TPictureViewScroller = class(TCustomControl)
  private
    FTracking: Boolean;
    FFullRedraw: Boolean;
    FHScrollVisible: Boolean;
    FVScrollVisible: Boolean;
    FOnScrolled: TNotifyEvent;
    function GetVScrollPos: Integer;
    procedure SetVScrollPos(Pos: Integer);
    function GetVScrollMax: Integer;
    procedure SetVScrollVisible(vis: Boolean);
    procedure SetHScrollVisible(vis: Boolean);
  protected
    HPos, VPos, XSize, YSize: Integer;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure UpdateScrollBars(XS, YS: Integer);
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetVPos(p: Integer);
    procedure SetHPos(p: Integer);
    procedure Paint; override;
    procedure ScrollChildren(dx, dy: Integer);
    procedure UpdateChildren;
    property FullRedraw: Boolean read FFullRedraw write FFullRedraw;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);override;

    procedure OnScrolled;virtual;

    procedure ScrollTo(y: Integer);
    property VScrollPos: Integer read GetVScrollPos write SetVScrollPos;
    property VScrollMax: Integer read GetVScrollMax;

  published
    { Published declarations }
    property Visible;
    property TabStop;
    property TabOrder;
    property Align;
    property HelpContext;
    property Tracking: Boolean read FTracking write FTracking;
    property VScrollVisible: Boolean read FVScrollVisible write SetVScrollVisible;
    property HScrollVisible: Boolean read FHScrollVisible write SetHScrollVisible;
    property DoOnScrolled: TNotifyEvent read FOnScrolled write FOnScrolled;
  end;

procedure Tag2Y(AControl: TControl);

implementation
{------------------------------------------------------}
procedure Tag2Y(AControl: TControl);
begin
    if AControl.Tag>10000 then
     AControl.Top := 10000
    else
       if AControl.Tag<-10000 then
         AControl.Top := -10000
       else
         AControl.Top := AControl.Tag;
end;
{------------------------------------------------------}
constructor TPictureViewScroller.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 TabStop := True;
 FTracking := true;
 FFullRedraw := False;
 FVScrollVisible := True;
 FHScrollVisible := True;
end;
{------------------------------------------------------}
procedure TPictureViewScroller.CreateParams(var Params: TCreateParams);
begin
  inherited   CreateParams(Params);   //CreateWindow
  Params.Style := Params.Style or WS_CLIPCHILDREN or WS_HSCROLL or WS_VSCROLL;
end;
{------------------------------------------------------}
procedure  TPictureViewScroller.CreateWnd;
begin
  inherited CreateWnd;
  VPos := 0;
  HPos := 0;
  UpdateScrollBars(ClientWidth, ClientHeight);
end;
{------------------------------------------------------}
procedure TPictureViewScroller.UpdateScrollBars(XS, YS: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  XSize := XS;
  YSize := YS;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);

//  HPos:=XSize;
//  VPos:=YSize;


  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := YSize-1;
  ScrollInfo.nPage := ClientHeight;
  ScrollInfo.nPos := VPos;
  ScrollInfo.nTrackPos := 0;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  if not FVScrollVisible then ShowScrollBar(Handle, SB_VERT, FVScrollVisible);

  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := XSize-1;
  ScrollInfo.nPage := ClientWidth;
  ScrollInfo.nPos := HPos;
  ScrollInfo.nTrackPos := 0;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
  if not FHScrollVisible then ShowScrollBar(Handle, SB_HORZ, FHScrollVisible);

end;
{------------------------------------------------------}
procedure TPictureViewScroller.UpdateChildren;
var i: Integer;
begin
    for i:=0 to ControlCount-1 do
      Tag2Y(Controls[i]);
end;
{------------------------------------------------------}
procedure TPictureViewScroller.ScrollChildren(dx, dy: Integer);
var i: Integer;
begin
  if (dx=0) and (dy=0) then exit;
  for i:=0 to ControlCount-1 do begin
   if dy<>0 then begin
    Controls[i].Tag := Controls[i].Tag+dy;
    Tag2Y(Controls[i]);
   end;
   if dx<>0 then Controls[i].Left := Controls[i].Left + dx;
  end
end;
{------------------------------------------------------}
procedure TPictureViewScroller.WMHScroll(var Message: TWMHScroll);
begin
  with Message do
    case ScrollCode of
      SB_LINEUP: SetHPos(HPos - 1);
      SB_LINEDOWN: SetHPos(HPos + 1);
      SB_PAGEUP: SetHPos(HPos-10);
      SB_PAGEDOWN: SetHPos(HPos+10);
//      SB_THUMBPOSITION: SetHPos(Pos);
      SB_THUMBTRACK: if FTracking then SetHPos(Pos);
      SB_TOP: SetHPos(0);
      SB_BOTTOM: SetHPos(XSize);
    end;

end;
{------------------------------------------------------}
procedure TPictureViewScroller.WMVScroll(var Message: TWMVScroll);
begin
  with Message do
    case ScrollCode of
      SB_LINEUP: SetVPos(VPos - 1);
      SB_LINEDOWN: SetVPos(VPos + 1);
      SB_PAGEUP: SetVPos(VPos-10);
      SB_PAGEDOWN: SetVPos(VPos+10);
//      SB_THUMBPOSITION: SetVPos(Pos);
      SB_THUMBTRACK: if FTracking then SetVPos(Pos);
      SB_TOP: SetVPos(0);
      SB_BOTTOM: SetVPos(YSize);
    end;

end;


{------------------------------------------------------}
procedure TPictureViewScroller.WMKeyDown(var Message: TWMKeyDown);
var vScrollNotify, hScrollNotify: Integer;
begin
  vScrollNotify := -1;
  hScrollNotify := -1;
  with Message do
    case CharCode of
        VK_UP:
            vScrollNotify := SB_LINEUP;
        VK_PRIOR:
            vScrollNotify := SB_PAGEUP;
        VK_NEXT:
            vScrollNotify := SB_PAGEDOWN;
        VK_DOWN:
            vScrollNotify := SB_LINEDOWN;
        VK_HOME:
            vScrollNotify := SB_TOP;
        VK_END:
            vScrollNotify := SB_BOTTOM;
        VK_LEFT:
            hScrollNotify := SB_LINELEFT;
        VK_RIGHT:
            hScrollNotify := SB_LINERIGHT;
    end;
  if (vScrollNotify <> -1) then
        Perform(WM_VSCROLL, vScrollNotify, 0);
  if (hScrollNotify <> -1) then
        Perform(WM_HSCROLL, hScrollNotify, 0);
  inherited;
end;
{------------------------------------------------------}
procedure TPictureViewScroller.SetVPos(p: Integer);
var   ScrollInfo: TScrollInfo;
      oldPos: Integer;
      r: TRect;
begin
  OldPos := VPos;
  VPos := p;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nPos := VPos;
  ScrollInfo.fMask := SIF_POS;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  GetScrollInfo(Handle, SB_VERT, ScrollInfo);
  VPos := ScrollInfo.nPos;
  r := ClientRect;
  if OldPos-VPos <> 0 then begin
   if FFullRedraw then begin
         ScrollChildren(0, (OldPos-VPos));
         Refresh;
       end
   else begin
         ScrollWindowEx(Handle, 0, (OldPos-VPos), nil, @r, 0, nil, SW_INVALIDATE {or
                   SW_SCROLLCHILDREN});
         ScrollChildren(0, (OldPos-VPos));
       end;
    OnScrolled;
  end;
end;
{------------------------------------------------------}
procedure TPictureViewScroller.SetHPos(p: Integer);
var   ScrollInfo: TScrollInfo;
      oldPos: Integer;
      r: TRect;
begin
  OldPos := HPos;
  HPos := p;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nPos := HPos;
  ScrollInfo.fMask := SIF_POS;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
  GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
  HPos := ScrollInfo.nPos;
  r := ClientRect;
  if OldPos-HPos <> 0 then begin
   if FFullRedraw then begin
         ScrollChildren((OldPos-HPos), 0);
         Refresh;
       end
   else begin
         ScrollWindowEx(Handle, (OldPos-HPos), 0,  nil, @r, 0, nil, SW_INVALIDATE{or
                   SW_SCROLLCHILDREN});
         ScrollChildren((OldPos-HPos), 0);
       end;
   OnScrolled;
  end;
end;
{------------------------------------------------------}
procedure TPictureViewScroller.Paint;
begin
 Canvas.Font.Color := clRed;
 Canvas.Font.Size := 2;
 Canvas.FillRect(Canvas.ClipRect);
end;
{------------------------------------------------------}
procedure TPictureViewScroller.ScrollTo(y: Integer);
begin
    SetVPos(y);
end;
{-------------------------------------------------------}
function TPictureViewScroller.GetVScrollPos: Integer;
begin
  GetVScrollPos := VPos;
end;
{-------------------------------------------------------}
procedure TPictureViewScroller.SetVScrollPos(Pos: Integer);
begin
   SetVPos(Pos);
end;
{-------------------------------------------------------}
function TPictureViewScroller.GetVScrollMax: Integer;
var ScrollInfo: TScrollInfo;
begin
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nPos := HPos;
  ScrollInfo.fMask := SIF_RANGE or SIF_PAGE;
  GetScrollInfo(Handle, SB_VERT, ScrollInfo);
  GetVScrollMax := ScrollInfo.nMax - Integer(ScrollInfo.nPage-1);
end;
{-------------------------------------------------------}
procedure TPictureViewScroller.SetVScrollVisible(vis: Boolean);
begin
    FVScrollVisible := vis;
    ShowScrollBar(Handle, SB_VERT, vis);
end;
{-------------------------------------------------------}
procedure TPictureViewScroller.SetHScrollVisible(vis: Boolean);
begin
    FHScrollVisible := vis;
    ShowScrollBar(Handle, SB_HORZ, vis);
end;
{-------------------------------------------------------}
procedure TPictureViewScroller.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TPictureViewScroller.OnScrolled;
begin
  if Assigned(FOnScrolled) then  FOnScrolled(Self);
end;

end.
