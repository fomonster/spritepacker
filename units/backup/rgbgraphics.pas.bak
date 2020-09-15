{
 /***************************************************************************
                                  RGBGraphics.pas


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author:  Tom Gregorovic (_tom_@centrum.cz)

  Abstract:
    TRGB32Bitmap is a memory image which allows fast pixel manipulations.
    TRGB32Canvas is a TRGB32Bitmap canvas for drawing primitives and
      drawing bitmap image into TCanvas.
}
unit RGBGraphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLProc, Interfaces, FPImage,
  IntfGraphics, Graphics, GraphType, Forms, Math,
  RGBTypes, RGBRoutines, RGBUtils, FPWritePNG, libwebp;
  

type
  TEraseMode = (emNone, emErase, emReplace);
  TDrawMode = (dmOutlineAndFill, dmOutline, dmFill);

  TRandomDensity = Word;
const
  MAXRANDOMDENSITY = $FFFF;
  
type
  { TRGB32Canvas }

  TRGB32Canvas = class
  private
    FDrawMode: TDrawMode;
    FEraseMode: TEraseMode;
    FFillColor: TRGB32Pixel;
    FFloodFillTolerance: TPixelDifference;
    FOutlineColor: TRGB32Pixel;
    FOwner: TRGB32BitmapCore;
    FPaperColor: TRGB32Pixel;
    FRandomDensity: TRandomDensity;
    FRandomEnabled: Boolean;
    FRectangleRoundness: Integer;
    function GetFillColor: TColor;
    function GetOutlineColor: TColor;
    function GetPaperColor: TColor;
    procedure SetFillColor(const AValue: TColor);
    procedure SetOutlineColor(const AValue: TColor);
    procedure SetPaperColor(const AValue: TColor);
  protected
    function SamePixelUnsafe(X, Y: Integer; Value: TRGB32Pixel): Boolean;

    procedure DrawOutlinePixel(X, Y: Integer);
    procedure DrawFillPixel(X, Y: Integer);
    procedure DrawPaperPixel(X, Y: Integer);
    
    procedure DrawReplacePixel(X, Y: Integer);

    procedure DrawRandomOutlinePixel(X, Y: Integer);
    procedure DrawRandomFillPixel(X, Y: Integer);
    procedure DrawRandomPaperPixel(X, Y: Integer);
    
    procedure DrawEmptyPixel(X, Y: Integer);
    
    function GetOutlineProcedure: TDrawPixelProcedure; virtual;
    function GetFillProcedure: TDrawPixelProcedure; virtual;
  public
    constructor Create(AOwner: TRGB32BitmapCore);

    procedure SetColor(X, Y: Integer; Value: TColor);
    function GetColor(X, Y: Integer): TColor;
    
    procedure Fill(Color: TColor);
    procedure FillRect(X1, Y1, X2, Y2: Integer);
    procedure Line(X1, Y1, X2, Y2: Integer);
    procedure Rectangle(X1, Y1, X2, Y2: Integer);
    procedure Ellipse(X1, Y1, X2, Y2: Integer);
    procedure FloodFill(X, Y: Integer);
  public
    procedure DrawTo(ACanvas: TCanvas; X, Y: Integer);
    procedure StretchDrawTo(ACanvas: TCanvas; DstX, DstY, DstWidth, DstHeight: Integer);
  public
    property EraseMode: TEraseMode read FEraseMode write FEraseMode;
    property DrawMode: TDrawMode read FDrawMode write FDrawMode;
    property FloodFillTolerance: TPixelDifference read FFloodFillTolerance
      write FFloodFillTolerance;
    
    property FillColor: TColor read GetFillColor write SetFillColor;
    property OutlineColor: TColor read GetOutlineColor write SetOutlineColor;
    property PaperColor: TColor read GetPaperColor write SetPaperColor;

    property RandomEnabled: Boolean read FRandomEnabled write FRandomEnabled;
    property RandomDensity: TRandomDensity read FRandomDensity write FRandomDensity;
    
    property RectangleRoundness: Integer read FRectangleRoundness write FRectangleRoundness;
  end;
  
  TSmoothMethod = (smAreaPixel, smBilinear, smBicubic);

  { TRGB32Bitmap }

  TRGB32Bitmap = class(TRGB32BitmapCore)
  private
    FCanvas: TRGB32Canvas;
  public
    constructor Create(AWidth, AHeight: Integer); override;
    constructor CreateAsCopy(ABitmap: TRGB32BitmapCore); override;
    constructor CreateFromData(AData: Pointer; AWidth, AHeight: Integer); override;
    constructor CreateFromFile(const FileName: String);
    constructor CreateFromBitmap(ABitmap: TBitmap);
    destructor Destroy; override;

    procedure SaveToFileNew(const FileName: String);
    procedure SaveToFile(const FileName: String);
  public
    procedure Draw(X, Y: Integer; ABitmap: TRGB32Bitmap);
    procedure DrawSpecial(X, Y: Integer; ABitmap: TRGB32Bitmap; turn:Boolean);
    procedure DrawSpecialRect(X, Y: Integer; ABitmap: TRGB32Bitmap; SourceRectX:integer; SourceRectY:integer; SourceRectWidth:integer; SourceRectHeight:integer; turn:Boolean);

    procedure FlipHorz;
    procedure FlipVert;
    procedure Rotate90;
    procedure Rotate180;
    procedure Rotate270;
    
    procedure StretchTrunc(AWidth, AHeight: Integer);
    procedure StretchSmooth(AWidth, AHeight: Integer; Method: TSmoothMethod);
    
    procedure Invert;
    procedure Grayscale;
  public
    property Canvas: TRGB32Canvas read FCanvas;
  end;

implementation

{ TRGB32Bitmap }

procedure SaveToDDS(bmp:TRGB32BitMap;filename:String;PixelFormat:Integer;Transparent:boolean);
  const
    DDSD_CAPS        = $00000001;
   DDSD_HEIGHT      = $00000002;
   DDSD_WIDTH       = $00000004;
   DDSD_PITCH       = $00000008;
   DDSD_PIXELFORMAT = $00001000;
   DDSD_MIPMAPCOUNT = $00020000;
   DDSD_LINEARSIZE  = $00080000;
   DDSD_DEPTH       = $00800000;

   DDPF_ALPHAPIXELS = $00000001;
   DDPF_FOURCC      = $00000004;
   DDPF_RGB         = $00000040;

   DDSCAPS_COMPLEX  = $00000008;
   DDSCAPS_TEXTURE  = $00001000;
   DDSCAPS_MIPMAP   = $00400000;

   DDSCAPS2_CUBEMAP           = $00000200;
   DDSCAPS2_CUBEMAP_POSITIVEX = $00000400;
   DDSCAPS2_CUBEMAP_NEGATIVEX = $00000800;
   DDSCAPS2_CUBEMAP_POSITIVEY = $00001000;
   DDSCAPS2_CUBEMAP_NEGATIVEY = $00002000;
   DDSCAPS2_CUBEMAP_POSITIVEZ = $00004000;
   DDSCAPS2_CUBEMAP_NEGATIVEZ = $00008000;
   DDSCAPS2_VOLUME            = $00200000;

   FOURCC_DXT1 = $31545844; // 'DXT1'
   FOURCC_DXT3 = $33545844; // 'DXT3'
   FOURCC_DXT5 = $35545844; // 'DXT5'
  type
    TDDPIXELFORMAT = record
          dwSize,
          dwFlags,
          dwFourCC,
          dwRGBBitCount,
          dwRBitMask,
          dwGBitMask,
          dwBBitMask,
          dwRGBAlphaBitMask : Cardinal;
       end;

       TDDCAPS2 = record
          dwCaps1,
          dwCaps2 : Cardinal;
          Reserved : array[0..1] of Cardinal;
       end;

       TDDSURFACEDESC2 = record
          dwSize,
          dwFlags,
          dwHeight,
          dwWidth,
          dwPitchOrLinearSize,
          dwDepth,
          dwMipMapCount : Cardinal;
          dwReserved1 : array[0..10] of Cardinal;
          ddpfPixelFormat : TDDPIXELFORMAT;
          ddsCaps : TDDCAPS2;
          dwReserved2 : Cardinal;
       end;

       TDDSHeader = record
          Magic : Cardinal;
          SurfaceFormat : TDDSURFACEDESC2;
       end;

       TFOURCC = array[0..3] of char;
  var
    stream:TFileStream;
    magic : TFOURCC;
    header : TDDSHeader;
    i,j, rowSize : Integer;
begin
  stream:=TFileStream.Create(filename,fmOpenWrite or fmCreate);

  stream.Seek(0, soFromBeginning);

  FillChar(header, SizeOf(TDDSHeader), 0);
  magic:='DDS ';
  header.magic:=Cardinal(magic);
  with header.SurfaceFormat do begin

    dwSize:=124;
    dwFlags:=DDSD_CAPS +
             DDSD_PIXELFORMAT +
             DDSD_WIDTH +
             DDSD_HEIGHT +
             DDSD_PITCH;
    dwWidth:=bmp.Width;
    dwHeight:=bmp.Height;

    case PixelFormat of
      {$IFDEF MSWINDOWS}
      0:
        begin
          ddpfPixelFormat.dwFlags:=DDPF_RGB;
          ddpfPixelFormat.dwRGBBitCount:=24;
          ddpfPixelFormat.dwRBitMask:=$00FF0000;
          ddpfPixelFormat.dwGBitMask:=$0000FF00;
          ddpfPixelFormat.dwBBitMask:=$000000FF;
        end;
         {$ENDIF}
      1:
        begin
          ddpfPixelFormat.dwFlags:=DDPF_RGB;
          ddpfPixelFormat.dwRGBBitCount:=32;
          ddpfPixelFormat.dwRBitMask:=$00FF0000;
          ddpfPixelFormat.dwGBitMask:=$0000FF00;
          ddpfPixelFormat.dwBBitMask:=$000000FF;
          if Transparent then begin
            ddpfPixelFormat.dwFlags:=ddpfPixelFormat.dwFlags + DDPF_ALPHAPIXELS;
            ddpfPixelFormat.dwRGBAlphaBitMask:=$FF000000;
          end;
        end;
      else
         //error
      end;

      rowSize:=(ddpfPixelFormat.dwRGBBitCount div 8)*dwWidth;

      dwPitchOrLinearSize:=dwHeight*Cardinal(rowSize);

      ddsCaps.dwCaps1:=DDSCAPS_TEXTURE;
      stream.Write(header, SizeOf(TDDSHeader));

      for i:=0 to bmp.Height-1 do
        stream.Write(bmp.Pixels[bmp.Width*i],rowSize);

  end;
  stream.Free;
end;

procedure SaveToWEBP(bmp:TRGB32BitMap;filename:String);
  var
    webpOut:PByte;
    webpSize:integer;
    stream:TFileStream;
begin
  try
    stream:=TFileStream.Create(filename,fmOpenWrite or fmCreate);
    stream.Seek(0, soFromBeginning);
    webpSize := WebPEncodeBGRA(PByte(bmp.GetPixelPtr(0,0)), bmp.Width, bmp.Height, bmp.Width * 4, 80, webpOut);
    stream.Write(webpOut^,webpSize);
  finally
    //WebPFree(webpOut);
    stream.Free;
  end;

end;

constructor TRGB32Bitmap.Create(AWidth, AHeight: Integer);
begin
  inherited;
  FCanvas := TRGB32Canvas.Create(Self);
end;

constructor TRGB32Bitmap.CreateAsCopy(ABitmap: TRGB32BitmapCore);
begin
  inherited;
  FCanvas := TRGB32Canvas.Create(Self);
end;

constructor TRGB32Bitmap.CreateFromData(AData: Pointer; AWidth, AHeight: Integer);
begin
  inherited;
  FCanvas := TRGB32Canvas.Create(Self);
end;

constructor TRGB32Bitmap.CreateFromFile(const FileName: String);
var
  Picture: TPicture;
begin
  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(FileName);
    CreateFromBitmap(Picture.Bitmap);
  finally
    Picture.Free;
  end;
end;

constructor TRGB32Bitmap.CreateFromBitmap(ABitmap: TBitmap);
var
  Image: TLazIntfImage;
begin
  Image := ABitmap.CreateIntfImage;
  try
    CreateFromLazIntfImage(Image);
  finally
    Image.Free;
  end;
end;

destructor TRGB32Bitmap.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TRGB32Bitmap.SaveToFileNew(const FileName: String);
  var
    ext:AnsiString;
    pngWriter : TLazWriterPNG;
    bmpWriter : TLazWriterBMP;
    pic:TPicture;
    img : TLazIntfImage;
    x:integer;
    y:integer;

begin
  ext := LowerCase(ExtractFileExt(FileName));

  if ( ext = '.png' ) then begin
    pngWriter := TLazWriterPNG.create;
    pngWriter.UseAlpha := true; //<----- needed to get an alpha channel
    pngWriter.WordSized := false;
    pngWriter.GrayScale:=false;
    img := TLazIntfImage.Create( Width, Height, [riqfRGB, riqfAlpha]);
    try
      img.CreateData;
      for x:=0 to Width-1 do
        for y:=0 to Height-1 do
          PDword(img.PixelData + 4 * (x + y * Width ) )^ := self.GetPixelPtr(x,y)^;

      img.SaveToFile(FileName, pngWriter);
    finally
      img.Free;
      pngWriter.Free;
    end;
  end else if ( ext = '.webp' ) then begin
    SaveToWEBP(self, filename);
  end else if ( ext = '.dds' ) then begin
    SaveToDDS(self,filename,1,true);
  end else if ( ext = '.jpg' ) then begin

     pic:=TPicture.Create;
     try

       pic.Jpeg.SetSize(Width, Height);
       pic.Jpeg.CompressionQuality:=97;
       self.Canvas.DrawTo(pic.Jpeg.Canvas,0,0);
       pic.Jpeg.SaveToFile(FileName);

     finally
       pic.Free;
     end;

  end else if ( ext = '.bmp' ) then begin
    bmpWriter := TLazWriterBMP.create;
    bmpWriter.BitsPerPixel:=32;
    img := TLazIntfImage.Create( Width, Height, [riqfRGB, riqfAlpha]);
    try
      img.CreateData;
      for x:=0 to Width-1 do
        for y:=0 to Height-1 do
          PDword(img.PixelData + 4 * (x + y * Width ) )^ := self.GetPixelPtr(x,y)^;

      img.SaveToFile(FileName, bmpWriter);
    finally
      img.Free;
      pngWriter.Free;
    end;
  end;
end;

procedure TRGB32Bitmap.SaveToFile(const FileName: String);
var
  Ext: String;
  NewGraphic: TGraphic;
  GraphicClass: TGraphicClass;
begin
  Ext := ExtractFileExt(FileName);
  Delete(Ext, 1, 1); // delete '.'
  GraphicClass := GetGraphicClassForFileExtension(Ext);
  if GraphicClass = nil then Exit;

  NewGraphic := GraphicClass.Create;
  try
    if not (NewGraphic is TBitmap) then Exit;
    NewGraphic.Width := Width;
    NewGraphic.Height := Height;
    DrawRGB32Bitmap((NewGraphic as TBitmap).Canvas.Handle, 0, 0, 0, 0, Width, Height, Self);
    NewGraphic.SaveToFile(Filename);
  finally
    NewGraphic.Free;
  end;
end;

{
procedure TRGB32Bitmap.SaveToClipboard;
var
  Ext: String;
  NewGraphic: TGraphic;
  GraphicClass: TGraphicClass;
begin
  Ext := ExtractFileExt(FileName);
  Delete(Ext, 1, 1); // delete '.'
  GraphicClass := GetGraphicClassForFileExtension(Ext);
  if GraphicClass = nil then Exit;

  NewGraphic := GraphicClass.Create;
  try
    if not (NewGraphic is TBitmap) then Exit;
    NewGraphic.Width := Width;
    NewGraphic.Height := Height;
    DrawRGB32Bitmap((NewGraphic as TBitmap).Canvas.Handle, 0, 0, 0, 0, Width, Height, Self);
    NewGraphic.SaveToFile(Filename);
  finally
    NewGraphic.Free;
  end;
end;
}

procedure TRGB32Bitmap.DrawSpecial(X, Y: Integer; ABitmap: TRGB32Bitmap; turn:Boolean);
begin
  DrawRGB32BitmapSpecial(Self, X, Y, ABitmap, turn);
end;

procedure TRGB32Bitmap.DrawSpecialRect(X, Y: Integer; ABitmap: TRGB32Bitmap; SourceRectX:integer; SourceRectY:integer; SourceRectWidth:integer; SourceRectHeight:integer; turn:Boolean);
begin
  DrawRGB32BitmapSpecialRect(Self, X, Y, ABitmap, SourceRectX, SourceRectY, SourceRectWidth, SourceRectHeight, turn );
end;

procedure TRGB32Bitmap.Draw(X, Y: Integer; ABitmap: TRGB32Bitmap);
begin
  DrawRGB32Bitmap(Self, X, Y, ABitmap);
end;

procedure TRGB32Bitmap.FlipHorz;
begin
  FlipHorzRGB32Bitmap(Self);
end;

procedure TRGB32Bitmap.FlipVert;
begin
  FlipVertRGB32Bitmap(Self);
end;

procedure TRGB32Bitmap.Rotate90;
begin
  Rotate90CWRGB32Bitmap(Self);
end;

procedure TRGB32Bitmap.Rotate180;
begin
  Rotate180CWRGB32Bitmap(Self);
end;

procedure TRGB32Bitmap.Rotate270;
begin
  Rotate270CWRGB32Bitmap(Self);
end;

procedure TRGB32Bitmap.StretchTrunc(AWidth, AHeight: Integer);
var
  Result: TRGB32Bitmap;
begin
  if (AWidth = Width) and (AHeight = Height) then Exit;
  Result := TRGB32Bitmap.Create(AWidth, AHeight);
  try
    StretchRGB32BitmapTrunc(Result, Self);
    ReplaceWith(Result);
  except
    FreeAndNil(Result);
  end;
end;

procedure TRGB32Bitmap.StretchSmooth(AWidth, AHeight: Integer; Method: TSmoothMethod);
begin
  StretchTrunc(AWidth,AHeight);
end;

procedure TRGB32Bitmap.Invert;
begin
  InvertRGB32Bitmap(Self);
end;

procedure TRGB32Bitmap.Grayscale;
begin
  GrayscaleRGB32Bitmap(Self);
end;

{ TRGB32Canvas }

constructor TRGB32Canvas.Create(AOwner: TRGB32BitmapCore);
begin
  inherited Create;
  
  FOwner := AOwner;
  FRandomDensity := MAXRANDOMDENSITY;
  FFloodFillTolerance := 0;
  FRectangleRoundness := 0;
end;

procedure TRGB32Canvas.SetColor(X, Y: Integer; Value: TColor);
begin
  FOwner.SetPixelInline(X, Y, ColorToRGB32Pixel(Value));
end;

function TRGB32Canvas.GetColor(X, Y: Integer): TColor;
var
  P: PRGB32Pixel;
begin
  P := FOwner.GetPixelPtrInline(X, Y);
  if P <> nil then Result := RGB32PixelToColorInline(P^)
  else Result := clNone;
end;

function TRGB32Canvas.GetFillColor: TColor;
begin
  Result := RGB32PixelToColorInline(FFillColor);
end;

function TRGB32Canvas.GetOutlineColor: TColor;
begin
  Result := RGB32PixelToColorInline(FOutlineColor);
end;

function TRGB32Canvas.GetPaperColor: TColor;
begin
  Result := RGB32PixelToColorInline(FPaperColor);
end;

procedure TRGB32Canvas.SetFillColor(const AValue: TColor);
begin
  FFillColor := ColorToRGB32PixelInline(AValue);
end;

procedure TRGB32Canvas.SetOutlineColor(const AValue: TColor);
begin
  FOutlineColor := ColorToRGB32PixelInline(AValue);
end;

procedure TRGB32Canvas.SetPaperColor(const AValue: TColor);
begin
  FPaperColor := ColorToRGB32PixelInline(AValue);
end;

function TRGB32Canvas.SamePixelUnsafe(X, Y: Integer; Value: TRGB32Pixel): Boolean;
begin
  Result := RGB32PixelDifferenceInline(FOwner.GetPixelPtrUnsafeInline(X, Y)^, Value)
    <= FFloodFillTolerance;
end;

procedure TRGB32Canvas.DrawOutlinePixel(X, Y: Integer);
begin
  FOwner.SetPixelInline(X, Y, FOutlineColor);
end;

procedure TRGB32Canvas.DrawFillPixel(X, Y: Integer);
begin
  FOwner.SetPixelInline(X, Y, FFillColor);
end;

procedure TRGB32Canvas.DrawPaperPixel(X, Y: Integer);
begin
  FOwner.SetPixelInline(X, Y, FPaperColor);
end;

procedure TRGB32Canvas.DrawReplacePixel(X, Y: Integer);
var
  P: PRGB32Pixel;
begin
  P := FOwner.GetPixelPtrInline(X, Y);
  if (P <> nil) and (P^ = FFillColor) then P^ := FPaperColor;
end;

procedure TRGB32Canvas.DrawRandomOutlinePixel(X, Y: Integer);
begin
  if Random(MAXRANDOMDENSITY) < FRandomDensity then
    FOwner.SetPixelInline(X, Y, FOutlineColor);
end;

procedure TRGB32Canvas.DrawRandomFillPixel(X, Y: Integer);
begin
  if Random(MAXRANDOMDENSITY) < FRandomDensity then
    FOwner.SetPixelInline(X, Y, FFillColor);
end;

procedure TRGB32Canvas.DrawRandomPaperPixel(X, Y: Integer);
begin
  if Random(MAXRANDOMDENSITY) < FRandomDensity then
    FOwner.SetPixelInline(X, Y, FPaperColor);
end;

procedure TRGB32Canvas.DrawEmptyPixel(X, Y: Integer);
begin
  //
end;

function TRGB32Canvas.GetOutlineProcedure: TDrawPixelProcedure;
begin
  Result := @DrawOutlinePixel;
  exit;
  if not FRandomEnabled then
  begin
    case DrawMode of
    dmOutlineAndFill, dmOutline:
    begin
      case EraseMode of
      emNone: Result := @DrawOutlinePixel;
      emErase: Result := @DrawPaperPixel;
      emReplace: Result := @DrawReplacePixel;
      end;
    end;
    else
      Result := @DrawEmptyPixel;
    end;
  end
  else
  begin
    case EraseMode of
    emNone: Result := @DrawRandomFillPixel;
    emErase: Result := @DrawRandomPaperPixel;
    end;
  end;
end;

function TRGB32Canvas.GetFillProcedure: TDrawPixelProcedure;
begin
  Result := @DrawFillPixel;
  exit;
  if not FRandomEnabled then
  begin
    case DrawMode of
    dmOutlineAndFill, dmFill:
    begin
      case EraseMode of
      emNone: Result := @DrawFillPixel;
      emErase: Result := @DrawPaperPixel;
      emReplace: Result := @DrawReplacePixel;
      end;
    end;
    else
      Result := @DrawEmptyPixel;
    end;
  end
  else
  begin
    case EraseMode of
    emNone: Result := @DrawRandomFillPixel;
    emErase: Result := @DrawRandomPaperPixel;
    end;
  end;
end;

procedure TRGB32Canvas.Fill(Color: TColor);
var
  P: TRGB32Pixel;
begin
  P := ColorToRGB32Pixel(Color);
  FillDWORD(FOwner.Pixels^, FOwner.Height * FOwner.RowPixelStride, P);
end;

procedure TRGB32Canvas.FillRect(X1, Y1, X2, Y2: Integer);
begin
  FillPixelRect(X1, Y1, X2, Y2, GetFillProcedure);
end;

procedure TRGB32Canvas.Line(X1, Y1, X2, Y2: Integer);
begin
  LineBresenham(X1, Y1, X2, Y2, GetOutlineProcedure);
end;

procedure TRGB32Canvas.Rectangle(X1, Y1, X2, Y2: Integer);
var
  R1, R2: Integer;
begin
  R1 := Max(0, Succ(Abs(X2 - X1)) - RectangleRoundness);
  R2 := Max(0, Succ(Abs(Y2 - Y1)) - RectangleRoundness);
  EllipticRectangle(X1, Y1, X2, Y2, R1, R2, GetOutlineProcedure, GetFillProcedure);
end;

procedure TRGB32Canvas.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  EllipticRectangle(X1, Y1, X2, Y2, 0, 0, GetOutlineProcedure, GetFillProcedure);
end;

procedure TRGB32Canvas.FloodFill(X, Y: Integer);
begin
  case EraseMode of
  emNone: FloodFillScanLine(X, Y, FOwner.Width, FOwner.Height, @FOwner.GetPixelUnsafe,
    @SamePixelUnsafe, @DrawFillPixel);
  emErase: FloodFillScanLine(X, Y, FOwner.Width, FOwner.Height, @FOwner.GetPixelUnsafe,
    @SamePixelUnsafe, @DrawPaperPixel);
  end;
end;

procedure TRGB32Canvas.DrawTo(ACanvas: TCanvas; X, Y: Integer);
begin
  if ACanvas <> nil then
    with FOwner do
      DrawRGB32Bitmap(ACanvas.Handle, X, Y, 0, 0, Width, Height, FOwner);
end;

procedure TRGB32Canvas.StretchDrawTo(ACanvas: TCanvas; DstX, DstY, DstWidth,
  DstHeight: Integer);
begin
  if ACanvas <> nil then
    with FOwner do
      StretchDrawRGB32Bitmap(ACanvas.Handle, DstX, DstY, DstWidth, DstHeight,
        0, 0, Width, Height, FOwner);
end;

end.

