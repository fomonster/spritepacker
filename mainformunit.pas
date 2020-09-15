{*******************************************************************************

  Проект: SpritePacker
  Автор: Фомин С.С.

  Назначение модуля:

*******************************************************************************}
unit MainFormUnit;
{******************************************************************************}
{$mode objfpc}{$H+}
{******************************************************************************}
interface
{******************************************************************************}
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, ComCtrls, ExtDlgs, Menus, StdCtrls, ConstantsUnit,
  ProtectionUnit, uPSComponent, SpritePackerUnit, SSXMLUnit, LogFormUnit,
  SSPictureUnit, SSPictureView, AboutFormUnit, SettingsFormUnit, RGBGraphics,
  IntfGraphics, uPSRuntime;
{******************************************************************************}
type
{******************************************************************************}
  { TMainForm }

  TMainForm = class(TForm)
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    PageInfoEdit: TEdit;
    ImagePanel: TPanel;
    NewButton1: TSpeedButton;
    OpenButton1: TSpeedButton;
    PSScript: TPSScript;
    PublishButton: TSpeedButton;
    RemoveButton: TSpeedButton;
    LinkButton: TSpeedButton;
    ApplicationProperties1: TApplicationProperties;
    ImageList: TImageList;
    ImageListSmall: TImageList;
    ImagePanelTop: TPanel;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    MenuItem1: TMenuItem;
    EditMenu: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenu: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenButton: TSpeedButton;
    SaveButton1: TSpeedButton;
    SaveButton2: TSpeedButton;
    SaveButton3: TSpeedButton;
    SaveButton4: TSpeedButton;
    SettingsButton: TSpeedButton;
    SaveButton: TSpeedButton;
    RemoveMenu: TMenuItem;
    MenuItem5: TMenuItem;
    AddButton: TSpeedButton;
    SaveProjectAsMenu: TMenuItem;
    SaveProjectMenu: TMenuItem;
    OpenAtlasMenu: TMenuItem;
    NewAtlasMenu: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    NewButton: TSpeedButton;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    SpritesTreeView: TTreeView;
    ToolButton3: TToolButton;
    procedure AboutMenuClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure AddFolderButtonClick(Sender: TObject);
    procedure AddSpriteButtonClick(Sender: TObject);
    procedure ApplicationProperties1Activate(Sender: TObject);
    procedure ApplicationProperties1Deactivate(Sender: TObject);
    procedure ApplicationProperties1DropFiles(Sender: TObject;
      const FileNames: array of String);
    procedure AtlasPublishMenuClick(Sender: TObject);
    procedure AtlasSettingsButtonClick(Sender: TObject);
    procedure AtlasSettingsMenuClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure LinkButtonClick(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure NewButton1Click(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure OpenButton1Click(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure PSScriptAfterExecute(Sender: TPSScript);
    procedure PSScriptCompile(Sender: TPSScript);
    procedure PSScriptExecute(Sender: TPSScript);
    procedure RemoveButtonClick(Sender: TObject);
    procedure RemoveMenuClick(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure NewAtlasButtonClick(Sender: TObject);
    procedure NewAtlasMenuClick(Sender: TObject);
    procedure OpenAtlasButtonClick(Sender: TObject);
    procedure OpenAtlasMenuClick(Sender: TObject);
    procedure PublishButtonClick(Sender: TObject);
    procedure RemoveSpriteButtonClick(Sender: TObject);
    procedure SaveAtlasButtonClick(Sender: TObject);
    procedure SaveButton1Click(Sender: TObject);
    procedure SaveButton2Click(Sender: TObject);
    procedure SaveButton3Click(Sender: TObject);
    procedure SaveButton4Click(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SaveProjectAsMenuClick(Sender: TObject);
    procedure SaveProjectMenuClick(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
    procedure SpritesTreeViewDblClick(Sender: TObject);
    procedure SpritesTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }



  public
    { public declarations }

    spritePacker:TSpritePacker;
    spritePackerFileName:AnsiString;

    FastCallList:array of TObjectMethod;

    picture:TSSPicture;

    PictureView:TPictureView;

    currentPage:Integer;

    publishError:Boolean;

    showAlpha:Boolean;

    //
    procedure NewDocument;
    procedure SaveDocument;
    procedure SaveAsDocument;
    procedure OpenDocument;

    //
    procedure AddSprite;
    procedure RemoveSprite;
    procedure AddFolder;
    procedure AtlasSettings;
    procedure AtlasPublish;

    //
    procedure Help;
    procedure About;

    //
    procedure SaveToXMLFile(_filename: string);
    procedure LoadFromXMLFile(_filename: string);

    procedure showTree;
    procedure showPicture;

    procedure publishPicture;
    procedure publishAtlas;

    procedure addFastCall(method:TObjectMethod);

    procedure FixHoles(srcBitmap:TRGB32Bitmap);

    procedure FixHolesAlpha(bitmap:TRGB32BitMap; _x:Integer; _y:Integer; _trimX:Integer; _trimY:Integer; _width:Integer; _height:Integer; turn:Boolean);


  end;

var
  MainForm: TMainForm;
  dataExportStream:TMemoryStream;


  function ExtractFilePathA(const FileName: string): string;
  function ExtractFileExtA(const FileName: string): string;
  function ExtractFileNameWithoutExtA(const FileName: string): string;
  function ExtractFileNameA(const FileName: string): string;

implementation
   Uses SpritePackerNodeUnit, md5;
{$R *.lfm}


{*******************************************************************************
   Management
*******************************************************************************}
procedure TMainForm.NewDocument;
var
  r:    integer;
begin
  // Тестовое заполнение текущего проекта
  if not IsDocSaved then
  begin
    r := MessageDlg('Save file?',
      'New created file is not saved! Save it or continue edit?', mtConfirmation,
      mbYesNoCancel, 0);
    if r = mrCancel then
      exit;
    if r = mrYes then
    begin
      SaveDocument;
    end;
  end;
  //ShowClearProperties;


  spritePacker.StopThread;
  Timer1.Enabled:=false;
  spritePacker.Clear;

  //showTree;
  //showPicture;

  spritePackerFileName := '';

  IsDocSaved:= True;
  spritePacker.StartThread;
  Timer1.Enabled:=true;
end;
{******************************************************************************}
procedure TMainForm.SaveAsDocument;
begin
  //ShowClearProperties;
  Timer1.Enabled:=false;
  spritePacker.StopThread;
  SaveDialog1.Filter   := 'Sprite Packer File|*.sps';
  SaveDialog1.Options:=[];
  SaveDialog1.Filename:=OpenSaveFilePath+'\';
  SaveDialog1.InitialDir:=OpenSaveFilePath+'\';
  if SaveDialog1.Execute then
  begin
    if not FileExists(SaveDialog1.FileName) or
      (MessageDlg('Overwrite?', 'File exists! Are you sure overwrite it?',
      mtConfirmation, mbYesNo, 0) = mrYes) then
    begin
      OpenSaveFilePath:=ExtractFilePath(SaveDialog1.FileName);
      spritePackerFileName := SaveDialog1.FileName;
      SaveToXMLFile(UTF8ToAnsi(SaveDialog1.FileName));
      IsDocSaved  := True;
    end;
  end;
  spritePacker.StartThread;
  Timer1.Enabled:=true;
end;
{******************************************************************************}
procedure TMainForm.SaveDocument;
begin
  //ShowClearProperties;
  Timer1.Enabled:=false;
  spritePacker.StopThread;
  if spritePackerFileName = '' then
    SaveAsDocument
  else
    SaveToXMLFile(UTF8ToAnsi(spritePackerFileName));
  IsDocSaved := True;
  spritePacker.StartThread;
  Timer1.Enabled:=true;
end;
{******************************************************************************}
procedure TMainForm.OpenDocument;
var
  r: integer;
begin
  if not IsDocSaved then
  begin
    r := MessageDlg('Save file?','New created file is not saved! Save it or continue edit?', mtConfirmation,
      mbYesNoCancel, 0);
    if r = mrCancel then exit;
    if r = mrYes then begin
      SaveDocument;
    end;
  end;
  //ShowClearProperties;
  Timer1.Enabled:=false;
  spritePacker.StopThread;
  OpenDialog1.Filter   := 'Sprite Packer File|*.sps';
  OpenDialog1.Options:=[ofFileMustExist];
  OpenDialog1.FileName := OpenSaveFilePath+'\';
  OpenDialog1.InitialDir:=OpenSaveFilePath+'\';
  if OpenDialog1.Execute then
  begin
    OpenSaveFilePath:=ExtractFilePath(OpenDialog1.FileName);
    LoadFromXMLFile(UTF8ToAnsi(OpenDialog1.FileName));
    spritePackerFileName := OpenDialog1.FileName;
    IsDocSaved  := True;
  end;
  spritePacker.StartThread;
  Timer1.Enabled:=true;
end;
{******************************************************************************}
procedure TMainForm.AddSprite;
var
  i:integer;
  filename:AnsiString;
begin
  Timer1.Enabled:=false;
  spritePacker.StopThread;
  OpenDialog1.Filter   := 'Image files (*.png,*.jpg,*.bmp)|*.png;*.jpg;*.bmp';
  OpenDialog1.Options:=[ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.FileName := OpenPicturePath+'\';
  OpenDialog1.InitialDir := OpenPicturePath+'\';
  if OpenDialog1.Execute then
  begin
    for i:=0 to OpenDialog1.Files.Count-1 do begin
        filename := OpenDialog1.Files[i];
        spritePacker.AddFile(filename, nil);
    end;

    //showTree;
    //showPicture;
    IsDocSaved  := false;
  end;
  spritePacker.StartThread;
  Timer1.Enabled:=true;
end;
{******************************************************************************}
procedure TMainForm.RemoveSprite;
  var
   i:Integer;
   treeNode:TTreeNode;
   node:TSpritePackerNode;
begin
  Timer1.Enabled:=false;
  spritePacker.StopThread;

  for i:=0 to SpritesTreeView.Items.Count-1 do begin;
    treeNode := SpritesTreeView.Items[i];

    if ( treeNode.Selected ) then begin
      if ( SpritesTreeView.Selected <> nil ) and ( SpritesTreeView.Selected.Data <> nil ) then begin
        node:= TSpritePackerNode(SpritesTreeView.Selected.Data);
        spritePacker.Delete(node.id);
      end;
    end;
  end;
  //showTree;
  //showPicture;
  spritePacker.IsChanged:=true;
  spritePacker.StartThread;
  Timer1.Enabled:=true;
  IsDocSaved:=false;
end;
{******************************************************************************}
procedure TMainForm.AddFolder;
begin
  Timer1.Enabled:=false;
  spritePacker.StopThread;
  SelectDirectoryDialog1.FileName := OpenPicturePath+'\';
  SelectDirectoryDialog1.InitialDir := OpenPicturePath+'\';
  if SelectDirectoryDialog1.Execute then begin

    spritePacker.AddFile(SelectDirectoryDialog1.FileName, nil);

    //showTree;
    //showPicture;
    IsDocSaved  := false;
  end;
  spritePacker.StartThread;
  Timer1.Enabled:=true;
end;
{******************************************************************************}
procedure TMainForm.AtlasSettings;
begin
  SettingsForm.ShowModal;
end;
{******************************************************************************}
procedure TMainForm.AtlasPublish;
  var
    bakCurrentPage:integer;
    i:integer;
begin
  bakCurrentPage := currentPage;

  // Проверяем есть ли картинки
  if ( spritePacker.Count <= 0 ) then begin
    ClearLog;
    AddLog('Add more sprites!');
    ShowLog;
    exit;
  end;

  // Указано ли итоговое изображение
  if ( ( spritePacker.outputImageFileName = '' ) or   IsDirectoryWriteable(spritePacker.outputImageFileName) ) then begin
    ClearLog;
    AddLog('Check output image filename and path!');
    ShowLog;
    exit;
  end;

  // Указано ли итоговое изображение
  if ( ( spritePacker.outputDataFileName = '' ) or   IsDirectoryWriteable(spritePacker.outputDataFileName) ) then begin
    ClearLog;
    AddLog('Check output data filename and path!');
    ShowLog;
    exit;
  end;

  ClearLog;
  publishError := false;

  AddLog('Publishing...');
  spritePacker.MaxRectsSort;

  for i:=0 to spritePacker.pagesCount - 1 do begin
    currentPage := i;
    publishPicture;
    publishAtlas;
  end;

  if ( publishError ) then begin
    AddLog('Terminated.');
  end else begin
    AddLog('Success.');
  end;

  ShowLog;

  currentPage := bakCurrentPage;
end;
{******************************************************************************}
procedure TMainForm.SaveToXMLFile(_filename: string);
var
  xml:  TXMLNode;
  Node: TXMLNode;
begin
  //ClearLog;

  xml      := TXMLNode.Create;
  Node      := xml.SubNodes_Add;
  Node.Name := 'SpritePacker';
  Node.Attribute_SetStr('filename', _filename);
  // Правка путей (в случае переноса sps файла в другое место)
  spritePacker.updatePaths(_filename);
  spritePacker.ToXML(Node);
  xml.SaveToXMLFile(_filename);
  xml.Destroy;

  //ShowLog;
end;
{******************************************************************************}
procedure TMainForm.LoadFromXMLFile(_filename: string);
var
  i:integer;
  xml, xml1:  TXMLNode;
  Node: TSpritePackerNode;
  xmlRoot: TXMLNode;
  oldFileName:String;
begin
  xml := TXMLNode.Create;
  xml.LoadFromXMLFile(_filename);
  oldFileName := '';
  xmlRoot := xml['SpritePacker'];
  if ( xmlRoot <> nil ) then begin
    spritePacker.filename:=xmlRoot.Attribute_Str('filename');
  end;
  spritePacker.FromXML(xml);
  spritePacker.updatePaths(_filename);
  xml.Destroy;
end;

{*******************************************************************************
   События формы
*******************************************************************************}

procedure TMainForm.FormCreate(Sender: TObject);
//var
  //node: TSpritePackerNode;

begin
  dataExportStream := TMemoryStream.Create;
  picture := nil;
  //
  PictureView:=TPictureView.Create(ImagePanelTop);
  ImagePanelTop.InsertControl(PictureView);

  // Определяем путь к программе
  path := ExtractFilePath(ParamStr(0));

  //DateSeparator:='.';
  //DecimalSeparator  := '.';

  //copyxml:=TXMLNode.Create;
  //copyxml.IsNotEmpty:=false;

  IsDocSaved := True;
  showAlpha := true;
  //SelectedNode    := nil;
  //OldSelectedNode := nil;
  MainForm.AllowDropFiles:=true;
  MainForm.DoubleBuffered   := True;
//  ScrollBox1.DoubleBuffered := True;
  //PanelLeft.DoubleBuffered  := True;
  //PanelTop.DoubleBuffered   := True;
  //  TreeView1.DoubleBuffered:=true;

  // Инициализируем дерево генератора
  spritePacker := TSpritePacker.Create;

  spritePacker.Clear;
  showTree;
  showPicture;
  IsDocSaved:= True;
  spritePacker.StartThread;


  //
  if (trim(paramstr(1))<>'') and FileExists(paramstr(1)) and (lowercase(ExtractFileExt(paramstr(1)))='.sps') then begin

    Timer1.Enabled:=false;
    spritePacker.StopThread;

    OpenSaveFilePath:=ExtractFilePath(paramstr(1));
    LoadFromXMLFile(UTF8ToAnsi(paramstr(1)));
    spritePackerFileName := paramstr(1);
    IsDocSaved  := True;

    Timer1Timer(nil);

    spritePacker.StartThread;
    Timer1.Enabled:=true;

  end else begin
    NewDocument;
  end;

end;
{******************************************************************************}
procedure TMainForm.showTree;

  procedure showTreeNode(packerNode:TSpritePackerNode; ownerTreeNode:TTreeNode);
    var
      i:integer;
      treeNode:TTreeNode;
      node:TSpritePackerNode;

  begin
    treeNode := SpritesTreeView.Items.AddChild(ownerTreeNode, packerNode.filename);
    treeNode.ImageIndex:=packerNode.typeId-1;
    treeNode.Data:=pointer(packerNode);
    treeNode.SelectedIndex:=packerNode.typeId-1;
    for i:=0 to spritePacker.Count-1 do begin
      node := spritePacker.items[i];
      if ( node.parentId = packerNode.id ) then begin
        showTreeNode(node, treeNode);
      end;
    end;
  end;

  var
    i:integer;
    packerNode:TSpritePackerNode;
begin
  SpritesTreeView.Items.Clear;

  for i:=0 to spritePacker.Count-1 do begin
     packerNode := spritePacker.items[i];
     if ( packerNode.parentId = -1 ) then begin

        showTreeNode(packerNode, nil );

     end;
  end;

end;
{******************************************************************************}
function PowerOfTwo(Value:Integer):Integer;
  var
    v:integer;
begin
  v:=1;
  while (v < value) do v := v shl 1;
  result := v;
end;
{******************************************************************************}
procedure TMainForm.Help;
begin

end;
{******************************************************************************}
procedure TMainForm.About;
begin
  AboutForm.ShowModal;
end;
{******************************************************************************}
procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
    r:Integer;
begin
  if not IsDocSaved then
  begin
    r := MessageDlg('Save file?','New created file is not saved! Save it or continue edit?', mtConfirmation,mbYesNoCancel, 0);
    if r = mrCancel then begin
      CloseAction := caNone;
      exit;
    end;
    if r = mrYes then SaveDocument;
  end;
  CloseAction := caFree;
  SaveConfig;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled:=false;

  dataExportStream.Destroy;
  dataExportStream:=nil;

  PictureView.CurrentPicture:=nil;
  picture.Destroy;
  picture:=nil;

  spritePacker.StopThread;
  spritePacker.Destroy;
  spritePacker:=nil;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  //if not CheckProgramInstallation then begin

  //end;
  LoadConfig;
end;

procedure TMainForm.HelpButtonClick(Sender: TObject);
begin
  ShowLog('');
end;

procedure TMainForm.LinkButtonClick(Sender: TObject);
begin
  AddFolder;
end;

procedure TMainForm.MenuItem10Click(Sender: TObject);
begin
//  MakeRegistrationSerial;
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
begin
  AddSprite;
end;

procedure TMainForm.MenuItem4Click(Sender: TObject);
begin
//  MakeRegistrationSerial;
end;

procedure TMainForm.MenuItem6Click(Sender: TObject);
begin
//  MakeRegistration;
end;

procedure TMainForm.MenuItem7Click(Sender: TObject);
begin
  AtlasPublish;
end;

procedure TMainForm.MenuItem9Click(Sender: TObject);
begin
  AtlasSettings;
end;

procedure TMainForm.NewButton1Click(Sender: TObject);
begin
  PictureView.TargetZoom(ImagePanel.Width div 2,ImagePanel.Height div 2, PictureView.CurrentPicture.Scale * 1.5);
end;

procedure TMainForm.NewButtonClick(Sender: TObject);
begin
  NewDocument;
end;

procedure TMainForm.OpenButton1Click(Sender: TObject);
begin
    PictureView.TargetZoom(ImagePanel.Width div 2,ImagePanel.Height div 2,PictureView.CurrentPicture.Scale * 0.5);
end;

procedure TMainForm.OpenButtonClick(Sender: TObject);
begin
  OpenDocument;
end;



procedure TMainForm.RemoveButtonClick(Sender: TObject);
begin
  RemoveSprite;
end;

procedure TMainForm.RemoveMenuClick(Sender: TObject);
begin
  RemoveSprite;
end;

procedure TMainForm.MenuItem5Click(Sender: TObject);
begin
  AddFolder;
end;

{******************************************************************************}
procedure TMainForm.NewAtlasButtonClick(Sender: TObject);
begin

end;

procedure TMainForm.NewAtlasMenuClick(Sender: TObject);
begin
  NewDocument;
end;

procedure TMainForm.OpenAtlasButtonClick(Sender: TObject);
begin

end;

procedure TMainForm.OpenAtlasMenuClick(Sender: TObject);
begin
  OpenDocument;
end;

procedure TMainForm.SaveAtlasButtonClick(Sender: TObject);
begin

end;

procedure TMainForm.SaveButton1Click(Sender: TObject);
begin
  PictureView.TargetZoom(ImagePanel.Width div 2,ImagePanel.Height div 2, 1);
end;

procedure TMainForm.SaveButton2Click(Sender: TObject);
begin
  currentPage:=currentPage-1;
  showPicture;
end;

procedure TMainForm.SaveButton3Click(Sender: TObject);
begin
  currentPage:=currentPage+1;
  showPicture;
end;

procedure TMainForm.SaveButton4Click(Sender: TObject);
begin
  //
  showAlpha := not showAlpha;
  showPicture;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  SaveDocument;
end;

procedure TMainForm.SaveProjectAsMenuClick(Sender: TObject);
begin
  SaveAsDocument;
end;

procedure TMainForm.SaveProjectMenuClick(Sender: TObject);
begin
  SaveDocument;
end;

procedure TMainForm.SettingsButtonClick(Sender: TObject);
begin
  AtlasSettings;
end;

procedure TMainForm.AddSpriteButtonClick(Sender: TObject);
begin
  AddSprite;
end;

procedure TMainForm.ApplicationProperties1Activate(Sender: TObject);
begin
  Timer1.Enabled:=true;
end;

procedure TMainForm.ApplicationProperties1Deactivate(Sender: TObject);
begin
  Timer1.Enabled:=false;
end;

procedure TMainForm.AddFolderButtonClick(Sender: TObject);
begin
  AddFolder;
end;

procedure TMainForm.AboutMenuClick(Sender: TObject);
begin
  About;
  //ShowLog;
end;

procedure TMainForm.AddButtonClick(Sender: TObject);
begin
  AddSprite;
end;

procedure TMainForm.RemoveSpriteButtonClick(Sender: TObject);
begin
  RemoveSprite;
end;

procedure TMainForm.PublishButtonClick(Sender: TObject);
begin
  AtlasPublish;
end;

procedure TMainForm.AtlasSettingsButtonClick(Sender: TObject);
begin
  AtlasSettings;
end;

procedure TMainForm.AtlasSettingsMenuClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.BitBtn1Click(Sender: TObject);
begin

end;

procedure TMainForm.ExitMenuClick(Sender: TObject);
begin
  //Quit;
end;

procedure TMainForm.ApplicationProperties1DropFiles(Sender: TObject; const FileNames: array of String);
var
   filename:AnsiString;
   fileAttributes:integer;
   i:integer;
begin
  Timer1.Enabled:=false;
  spritePacker.StopThread;
  for i:=0 to Length(FileNames)-1 do begin
     filename := FileNames[i];

     fileAttributes := FileGetAttr(filename);

     if ((fileAttributes and faDirectory) > 0 ) then begin
       spritePacker.AddFile(filename, nil);
     end else begin
       if FileExists(filename) then begin
          spritePacker.AddFile(filename, nil);
       end;
     end;

     IsDocSaved:=false;
  end;
  //showTree;
  //showPicture;

  spritePacker.StartThread;
  Timer1.Enabled:=true;
end;

procedure TMainForm.AtlasPublishMenuClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.SpritesTreeViewDblClick(Sender: TObject);
  var
    node:TSpritePackerNode;
begin
  if ( SpritesTreeView.Selected = nil ) or ( SpritesTreeView.Selected.Data = nil ) then exit;
  node:= TSpritePackerNode(SpritesTreeView.Selected.Data);

  ShowLog('filename: ' + node.filename + #13 + #10 +
          'filepath: ' + node.filepath + #13 + #10 +
          'path: ' + node.path + #13 + #10 +
          'sps path: '+ExtractFilePath(spritePackerFileName));
end;
{******************************************************************************}
procedure TMainForm.SpritesTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  var

  Node: TTreeNode;

begin

  // Проверка нажития кнопки мышки на картинках текстур
  Node := SpritesTreeView.GetNodeAt(X, Y);
  //if not CheckBox1.Checked then exit;
  if Node <> nil then begin



  end;
end;
{******************************************************************************}
procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=false;
  // Периодическое выполнение по таймеру
  if ( Length(FastCallList) > 0 ) then begin
    FastCallList[Length(FastCallList)-1]();
    SetLength(FastCallList,Length(FastCallList)-1);
  end else begin
     spritePacker.FrameMove;
     if ( spritePacker.IsChanged and not spritePacker.IsLoadedOnThisFrame ) then begin
       showTree;
       showPicture;
       spritePacker.IsChanged := false;
     end;
  end;
  Timer1.Enabled:=true;
end;
{******************************************************************************}

procedure TMainForm.addFastCall(method:TObjectMethod);
begin
  SetLength(FastCallList,Length(FastCallList)+1);
  FastCallList[Length(FastCallList)-1]  := method;
end;

{******************************************************************************}
procedure TMainForm.showPicture;
  var
    i, j,x,y:integer;
    c:DWord;
    _width,_height, margin, nodeWidth, nodeHeight:Integer;
    node:TSpritePackerNode;

begin

  spritePacker.MaxRectsSort;

  //
  if ( currentPage < 0 ) then currentPage:=0;
  if ( currentPage >= spritePacker.pagesCount - 1 ) then currentPage := spritePacker.pagesCount-1;

  //
  PageInfoEdit.Text := intToStr(currentPage+1) + '/'+intToStr(spritePacker.pagesCount);


  _width:=spritePacker.maxWidth;
  _height:=spritePacker.maxHeight;

  //
  if spritePacker.clampWidthAndHeight = 0 then begin
    // minimal
    _width := 0;
    _height := 0;
    margin := 0;
    if ( spritePacker.postProcess > 0 ) or ( spritePacker.spritesMargin > 0 ) then margin:=1;
    for i:=0 to spritePacker.Count-1 do begin
      node:=spritePacker[i];

      if spritePacker.spritesOptimization > 0 then begin
        nodeWidth:=node.trimMaxX - node.trimMinX + 1;
        nodeHeight:=node.trimMaxY - node.trimMinY + 1;
      end else begin
        nodeWidth:=node.width;
        nodeHeight:=node.height;
      end;

      if ( node.page = currentPage ) then begin
        if ( node.turn ) then begin
          if ( _width < node.x + nodeHeight + margin ) then _width := node.x + nodeHeight + margin;
          if ( _height < node.y + nodeWidth + margin ) then _height := node.y + nodeWidth + margin;
        end else begin
          if ( _width < node.x + nodeWidth + margin ) then _width := node.x + nodeWidth + margin;
          if ( _height < node.y + nodeHeight + margin ) then _height := node.y + nodeHeight + margin;
        end;
      end;
    end;

  end else if spritePacker.clampWidthAndHeight = 1 then begin
    // power 2
    _width := 0;
    _height := 0;
    margin := 0;
    if ( spritePacker.postProcess > 0 ) or ( spritePacker.spritesMargin > 0 ) then margin:=1;
    for i:=0 to spritePacker.Count-1 do begin
      node:=spritePacker[i];

      if spritePacker.spritesOptimization > 0 then begin
        nodeWidth:=node.trimMaxX - node.trimMinX + 1;
        nodeHeight:=node.trimMaxY - node.trimMinY + 1;
      end else begin
        nodeWidth:=node.width;
        nodeHeight:=node.height;
      end;

      if ( node.page = currentPage ) then begin
        if ( node.turn ) then begin
          if ( _width < node.x + nodeHeight + margin ) then _width := node.x + nodeHeight + margin;
          if ( _height < node.y + nodeWidth + margin ) then _height := node.y + nodeWidth + margin;
        end else begin
          if ( _width < node.x + nodeWidth + margin ) then _width := node.x + nodeWidth + margin;
          if ( _height < node.y + nodeHeight + margin ) then _height := node.y + nodeHeight + margin;
        end;
      end;
    end;

    //
    _width:=PowerOfTwo(_width);
    _height:=PowerOfTwo(_height);

  end;

  if ( _width <= 0 ) then _width := spritePacker.maxWidth;
  if ( _height <= 0 ) then _height := spritePacker.maxHeight;

  //
  if ( picture = nil ) then begin
    picture:=TSSPicture.Create;
  end;
  picture.SetSize(_width,_height);
  picture.bitmap.Canvas.Fill($00000000);

  for i:=0 to spritePacker.Count-1 do begin
    node:=spritePacker[i];
    if ( node.page = currentPage ) then begin
      if ( node.bitmap <> nil ) then begin
        if ( spritePacker.spritesOptimization > 0 ) then begin
          if ( node.turn ) then begin
            picture.bitmap.DrawSpecialRect(node.x, node.y, node.bitmap, node.trimMinX, node.trimMinY, node.trimMaxX - node.trimMinX + 1, node.trimMaxY - node.trimMinY + 1, node.turn);
          end else begin
            picture.bitmap.DrawSpecialRect(node.x, node.y, node.bitmap, node.trimMinX, node.trimMinY, node.trimMaxX - node.trimMinX + 1, node.trimMaxY - node.trimMinY + 1, node.turn);
          end;
        end else begin
          picture.bitmap.DrawSpecial(node.x,node.y,node.bitmap, node.turn);
        end;
      end;
    end;
  end;

  // Post process
  if ( spritePacker.postProcess > 0 ) then begin
    FixHoles(picture.bitmap);

    // alpha fix
    for i:=0 to spritePacker.Count-1 do begin
      node:=spritePacker[i];
      if ( node.page = currentPage ) then begin
        if ( node.bitmap <> nil ) then begin
          if ( spritePacker.spritesOptimization > 0 ) then begin
            if ( node.turn ) then begin
              FixHolesAlpha(picture.bitmap, node.x, node.y, node.trimMinX, node.trimMinY, node.trimMaxX - node.trimMinX + 1, node.trimMaxY - node.trimMinY + 1, node.turn);
            end else begin
              FixHolesAlpha(picture.bitmap, node.x, node.y, node.trimMinX, node.trimMinY, node.trimMaxX - node.trimMinX + 1, node.trimMaxY - node.trimMinY + 1, node.turn);
            end;
          end else begin
            FixHolesAlpha(picture.bitmap, node.x,node.y, 0, 0, node.bitmap.Width, node.bitmap.Height, node.turn);
          end;
        end;
      end;
    end;

  end;

  if not showAlpha then begin

    for y:=picture.bitmap.Height-1 downto 0 do begin
      for x:=0 to picture.bitmap.Width-1 do begin
         c:=picture.bitmap.GetPixelUnsafe(x, y);
         c := c or $ff000000;
         picture.bitmap.SetPixelUnsafe(x, y, c);
      end;
    end;

  end;

  PictureView.CurrentPicture:=picture;

  PictureView.RecalculateScrollBars;

  PictureView.Repaint;
  PictureView.SetMode_ZoomAndMove;

end;
{******************************************************************************}
procedure TMainForm.FixHolesAlpha(bitmap:TRGB32Bitmap; _x:Integer; _y:Integer; _trimX:Integer; _trimY:Integer; _width:Integer; _height:Integer; turn:Boolean);
  var
      i, j, x,y:integer;
      c:DWord;

   procedure fixAlpha(toX:integer;toY:integer;fromX:integer;fromY:Integer);
     var
       ca:DWord;
   begin
     if ( toX < 0 ) or ( toX >=bitmap.Width ) or ( toY < 0 ) or ( toY >=bitmap.Height ) then exit;

     c:=bitmap.GetPixelUnsafe(fromX, fromY);

     ca := c and $ff000000;
     c:=bitmap.GetPixelUnsafe(toX, toY);
     c := (c and $00ffffff) or ca;
     bitmap.SetPixelUnsafe(toX, toY, c);
   end;

begin
  if ( not turn ) then begin
    for y:=-1 to _height do begin
      fixAlpha(_x - 1, _y + y, { <<== } _x, _y + y);
      fixAlpha(_x + _width, _y + y, { <<== } _x + _width - 1, _y + y);
    end;
    for x:=-1 to _width do begin
      fixAlpha(_x + x, _y - 1, { <<== } _x + x, _y);
      fixAlpha(_x + x, _y + _height, { <<== } _x + x, _y + _height - 1);
    end;
  end else begin
    for y:=-1 to _width do begin
      fixAlpha(_x - 1, _y + y, { <<== } _x, _y + y);
      fixAlpha(_x + _height, _y + y, { <<== } _x + _height-1, _y + y);
    end;
    for x:=-1 to _height do begin
      fixAlpha(_x + x, _y - 1, { <<== } _x + x, _y);
      fixAlpha(_x + x, _y + _width, { <<== } _x + x, _y + _width - 1);
    end;
  end;

end;
{******************************************************************************}
procedure TMainForm.FixHoles(srcBitmap:TRGB32Bitmap);

  procedure FixPixel(bitmap:TRGB32Bitmap; dstBitmap:TRGB32Bitmap; x:integer; y:integer);
    var
      ca, cr,cg,cb, cnt, c1,c2,c3,c4,c5,c6,c7,c8,c9, eps:DWord;
      a1,a2,a3,a4,a5,a6,a7,a8,a9:DWord;
      fix:boolean;
  begin


    eps:=20 shl 24;
    c1:=0; c2:=0; c3:=0; c4:=0; c6:=0; c7:=0; c8:=0; c9:=0;

    if ( y > 0 ) then begin
      if ( x > 0 ) then c7:=bitmap.GetPixelUnsafe(x-1, y-1);
      c8:=bitmap.GetPixelUnsafe(x, y-1);
      if ( x < bitmap.Width-1 ) then c9:=bitmap.GetPixelUnsafe(x+1, y-1);
    end;

    if ( x > 0 ) then c4:=bitmap.GetPixelUnsafe(x-1, y);
    c5:=bitmap.GetPixelUnsafe(x, y);
    if ( x < bitmap.Width-1 ) then c6:=bitmap.GetPixelUnsafe(x+1, y);

    if ( y < bitmap.Height-1 ) then begin
      if ( x > 0 ) then c1:=bitmap.GetPixelUnsafe(x-1, y+1);
      c2:=bitmap.GetPixelUnsafe(x, y+1);
      if ( x < bitmap.Width-1 ) then c3:=bitmap.GetPixelUnsafe(x+1, y+1);
    end;

    a1:=( c1 and $ff000000 ) shr 24;
    a2:=( c2 and $ff000000 ) shr 24;
    a3:=( c3 and $ff000000 ) shr 24;
    a4:=( c4 and $ff000000 ) shr 24;
    a5:=( c5 and $ff000000 ) shr 24;
    a6:=( c6 and $ff000000 ) shr 24;
    a7:=( c7 and $ff000000 ) shr 24;
    a8:=( c8 and $ff000000 ) shr 24;
    a9:=( c9 and $ff000000 ) shr 24;

    fix := true;
    //fix := (a1>200) or (a2>200) or (a3>200) or (a4>200) or (a6>200) or (a7>200) or (a8>200) or (a9>200);
    //fix := (a2>254) or (a4>254) or (a6>254) or (a8>254);

    if (( c5 and $ff000000 ) = 0 ) then begin

      cnt:=0;
      ca:=0;
      cr:=0;
      cg:=0;
      cb:=0;

      //
      if (( c1 and $ff000000 ) <> 0 ) then begin
        cnt:=cnt+1;
        ca:= ca + a1;
        cr:= cr + (( c1 and $ff0000 ) shr 16);
        cg:= cg + (( c1 and $ff00 ) shr 8);
        cb:= cb + (( c1 and $ff ));
      end;
      //
      if (( c2 and $ff000000 ) <> 0 ) then begin
        cnt:=cnt+1;
        ca:= ca + a2;
        cr:= cr + (( c2 and $ff0000 ) shr 16);
        cg:= cg + (( c2 and $ff00 ) shr 8);
        cb:= cb + (( c2 and $ff ));
      end;
      //
      if (( c3 and $ff000000 ) <> 0 ) then begin
        cnt:=cnt+1;
        ca:= ca + a3;
        cr:= cr + (( c3 and $ff0000 ) shr 16);
        cg:= cg + (( c3 and $ff00 ) shr 8);
        cb:= cb + (( c3 and $ff ));
      end;
      //
      if (( c4 and $ff000000 ) <> 0 ) then begin
        cnt:=cnt+1;
        ca:= ca + a4;
        cr:= cr + (( c4 and $ff0000 ) shr 16);
        cg:= cg + (( c4 and $ff00 ) shr 8);
        cb:= cb + (( c4 and $ff ));
      end;
      //
      if (( c6 and $ff000000 ) <> 0 ) then begin
        cnt:=cnt+1;
        ca:= ca + a6;
        cr:= cr + (( c6 and $ff0000 ) shr 16);
        cg:= cg + (( c6 and $ff00 ) shr 8);
        cb:= cb + (( c6 and $ff ));
      end;
      //
      if (( c7 and $ff000000 ) <> 0 ) then begin
        cnt:=cnt+1;
        ca:= ca + a7;
        cr:= cr + (( c7 and $ff0000 ) shr 16);
        cg:= cg + (( c7 and $ff00 ) shr 8);
        cb:= cb + (( c7 and $ff ));
      end;
      //
      if (( c8 and $ff000000 ) <> 0 ) then begin
        cnt:=cnt+1;
        ca:= ca + a8;
        cr:= cr + (( c8 and $ff0000 ) shr 16);
        cg:= cg + (( c8 and $ff00 ) shr 8);
        cb:= cb + (( c8 and $ff ));
      end;
      //
      if (( c9 and $ff000000 ) <> 0 ) then begin
        cnt:=cnt+1;
        ca:= ca + a9;
        cr:= cr + (( c9 and $ff0000 ) shr 16);
        cg:= cg + (( c9 and $ff00 ) shr 8);
        cb:= cb + (( c9 and $ff ));
      end;
      ca := a5;
      if ( cnt > 0 ) and ( fix ) then begin
        c5:= (((ca div cnt ) and $ff ) shl 24) or (((cr div cnt ) and $ff ) shl 16) or (((cg div cnt ) and $ff ) shl 8) or (((cb div cnt ) and $ff ));
      end;

    end;
    dstBitmap.SetPixelUnsafe(x, y, c5);
  end;

  var
    x, y:integer;
    newBitmap:TRGB32Bitmap;
    c:DWord;
begin
  newBitmap := TRGB32Bitmap.Create(srcBitmap.Width, srcBitmap.Height);
  for y:=srcBitmap.Height-1 downto 0 do begin
    for x:=0 to srcBitmap.Width-1 do begin
      FixPixel(srcBitmap,newBitmap, x, y);
    end;
  end;
  for y:=srcBitmap.Height-1 downto 0 do begin
    for x:=0 to srcBitmap.Width-1 do begin
      c:=newBitmap.GetPixelUnsafe(x, y);
      //srcBitmap.SetPixelUnsafe(x, y, c);
      FixPixel(newBitmap, srcBitmap, x, y);
    end;
  end;
  newBitmap.Free;
end;
{******************************************************************************}
function psImageType(index:integer):integer;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].typeId else result:=-1;
end;

function psImageWidth(index:integer):integer;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].width else result:=0;
end;

function psImageHeight(index:integer):integer;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].height else result:=0;
end;

function psImageX(index:integer):integer;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].x else result:=0;
end;

function psImageY(index:integer):integer;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].y else result:=0;
end;

function psImageTrimMinX(index:integer):integer;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].trimMinX else result:=0;
end;

function psImageTrimMinY(index:integer):integer;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].trimMinY else result:=0;
end;

function psImageTrimMaxX(index:integer):integer;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].trimMaxX else result:=0;
end;

function psImageTrimMaxY(index:integer):integer;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].trimMaxY else result:=0;
end;

function psImageTurn(index:integer):boolean;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].turn else result:=false;
end;

function psImagePage(index:integer):integer;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].page else result:=-1;
end;

function psImagePath(index:integer):string;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].filepath else result:='';
end;

function psImageName(index:integer):string;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].filename else result:='';
end;

function psImageFullName(index:integer):string;
begin
  if ( index >= 0 ) and ( index < MainForm.spritePacker.Count ) then result:=MainForm.spritePacker[index].fullName else result:='';
end;
{******************************************************************************}
procedure psClear;
begin
  dataExportStream.Clear;
end;

procedure psSetPosition(position: integer);
begin
  if ( position < 0 ) then position:=0;
  if ( position >= dataExportStream.Size ) then position:=dataExportStream.Size-1;
  dataExportStream.Position:=position;
end;

procedure psWriteByte(const value: Byte);
begin
  dataExportStream.WriteByte(value);
end;

procedure psWriteWord(const data: Word);
begin
  dataExportStream.WriteWord(data);
end;

procedure psWriteInt(const value: integer);
begin
  dataExportStream.WriteDWord(Cardinal(value));
end;

procedure psWriteCardinal(const value: Cardinal);
begin
  dataExportStream.WriteDWord(Cardinal(value));
end;

procedure psWriteFloat(const value: single);
 var
   data:PDWord;
begin
  data := @value;
  dataExportStream.WriteDWord(data^);
end;

procedure psWriteDouble(const value: double);
 var
   data:PQWord;
begin
  data := @value;
  dataExportStream.WriteQWord(data^);
end;

procedure psWriteString(const value: ansistring);
begin
  dataExportStream.Write(value[1],length(value));
end;

{******************************************************************************}
function ExtractFilePathA(const FileName: string): string;
begin
  result := ExtractFilePath(FileName);
end;

function ExtractFileExtA(const FileName: string): string;
begin
  result := ExtractFileExt(FileName);
end;

function ExtractFileNameWithoutExtA(const FileName: string): string;
begin
  result := ExtractFileNameWithoutExt(FileName);
end;


function ExtractFileNameA(const FileName: string): string;
begin
  result := ExtractFileName(FileName);
end;

{******************************************************************************}
procedure TMainForm.PSScriptCompile(Sender: TPSScript);
begin
  Sender.AddRegisteredVariable('imagesCount','integer');
  Sender.AddRegisteredVariable('currentPage','integer');
  Sender.AddRegisteredVariable('outputImageWidth','integer');
  Sender.AddRegisteredVariable('outputImageHeight','integer');
  Sender.AddRegisteredVariable('outputImageClampedWidth','integer');
  Sender.AddRegisteredVariable('outputImageClampedHeight','integer');
  Sender.AddRegisteredVariable('outputImageFileName','string');
  Sender.AddRegisteredVariable('outputImageFileFormat','string');
  Sender.AddRegisteredVariable('outputDateFileName','string');
  Sender.AddRegisteredVariable('spritesOptimization','integer');
  Sender.AddRegisteredVariable('namesWithPath','integer');
  Sender.AddRegisteredVariable('namesWithExt','integer');

  Sender.AddFunction(@psClear,'procedure Clear;');
  Sender.AddFunction(@psSetPosition,'procedure setPosition(position: integer);');
  Sender.AddFunction(@psWriteByte,'procedure writeByte(const value: Byte);');
  Sender.AddFunction(@psWriteWord,'procedure writeWord(const value: Word);');
  Sender.AddFunction(@psWriteInt,'procedure writeInt(const value: integer);');
  Sender.AddFunction(@psWriteCardinal,'procedure writeCardinal(const value: integer);');
  Sender.AddFunction(@psWriteFloat,'procedure writeFloat(const value: Single);');
  Sender.AddFunction(@psWriteDouble,'procedure writeDouble(const value: double);');
  Sender.AddFunction(@psWriteString,'procedure writeString(const value: ansistring);');

  Sender.AddFunction(@psImageType, 'function imageType(const index: integer):integer;');
  Sender.AddFunction(@psImageWidth, 'function imageWidth(const index: integer):integer;');
  Sender.AddFunction(@psImageHeight,'function imageHeight(const index: integer):integer;');
  Sender.AddFunction(@psImageX,'function imageX(const index: integer):integer;');
  Sender.AddFunction(@psImageY,'function imageY(const index: integer):integer;');
  Sender.AddFunction(@psImageTrimMinX,'function imageTrimMinX(const index: integer):integer;');
  Sender.AddFunction(@psImageTrimMinY,'function imageTrimMinY(const index: integer):integer;');
  Sender.AddFunction(@psImageTrimMaxX,'function imageTrimMaxX(const index: integer):integer;');
  Sender.AddFunction(@psImageTrimMaxY,'function imageTrimMaxY(const index: integer):integer;');
  Sender.AddFunction(@psImageTurn,'function imageTurn(const index: integer):boolean;');
  Sender.AddFunction(@psImagePage,'function imagePage(const index: integer):integer;');
  Sender.AddFunction(@psImagePath,'function imagePath(const index: integer):string;');
  Sender.AddFunction(@psImageName,'function imageName(const index: integer):string;');
  Sender.AddFunction(@psImageFullName,'function imageFullName(const index: integer):string;');

  Sender.AddFunction(@showmessage,'procedure showmessage(const s: string);');
  Sender.AddFunction(@ExtractFilePathA,'function ExtractFilePath(const FileName: string): string;');
  Sender.AddFunction(@ExtractFileExtA,'function ExtractFileExt(const FileName: string): string;');
  Sender.AddFunction(@ExtractFileNameWithoutExtA,'function ExtractFileNameWithoutExt(const FileName: string): string;');
  Sender.AddFunction(@ExtractFileNameA,'function ExtractFileName(const FileName: string): string;');
end;

procedure TMainForm.PSScriptExecute(Sender: TPSScript);
  var
    filenameAddon:String;
begin
  filenameAddon := '';
  if ( currentPage > 0 ) then filenameAddon:='_'+IntToStr(currentPage);

  VSetString(Sender.GetVariable('outputImageFileName'), spritePacker.outputImageFileName + filenameAddon);
  VSetString(Sender.GetVariable('outputImageFileFormat'), spritePacker.outputImageFileFormat);
  VSetString(Sender.GetVariable('outputDateFileName'), spritePacker.outputDataFileName);
  VSetInt(Sender.GetVariable('outputImageWidth'), spritePacker.maxWidth);
  VSetInt(Sender.GetVariable('outputImageHeight'), spritePacker.maxHeight);
  VSetInt(Sender.GetVariable('outputImageClampedWidth'), spritePacker.currentPageClampedWidth);
  VSetInt(Sender.GetVariable('outputImageClampedHeight'), spritePacker.currentPageClampedHeight);
  VSetInt(Sender.GetVariable('spritesOptimization'), spritePacker.spritesOptimization);

  VSetInt(Sender.GetVariable('currentPage'), currentPage);
  VSetInt(Sender.GetVariable('imagesCount'), spritePacker.count);
  VSetInt(Sender.GetVariable('namesWithPath'), spritePacker.namesWithPath);
  VSetInt(Sender.GetVariable('namesWithExt'), spritePacker.namesWithExt);

end;


procedure TMainForm.PSScriptAfterExecute(Sender: TPSScript);

begin

end;

procedure TMainForm.publishAtlas;
  var
    stream:TFileStream;

    str:AnsiString;
    filenameAddon:AnsiString;
    exportScriptFilename:AnsiString;
    dataFileName:AnsiString;
begin
  dataExportStream.Clear;
  dataExportStream.Position:=0;

  try
    exportScriptFilename := ExtractFilePath(paramstr(0))+'export\'+spritePacker.outputDataFileFormat+'.psc';
    PSScript.Script.LoadFromFile(exportScriptFilename); // Скрипт экспортера
    if PSScript.compile then begin
       PSScript.execute;

       if ( spritePacker.outputDataFileName <> '' ) then begin

         filenameAddon := '';
         if ( currentPage > 0 ) then filenameAddon:='_'+IntToStr(currentPage);

         dataFileName:=spritePacker.outputDataFileName+filenameAddon+PSScript.ExecuteFunction([],'getFileExt');

         str:=PSScript.ExecuteFunction([],'getResult');
         AddLog(str);

         stream:=TFileStream.Create(dataFileName,fmOpenWrite or fmCreate);
         stream.Seek(0, soFromBeginning);
         dataExportStream.Position:=0;
         stream.CopyFrom(dataExportStream, dataExportStream.Size);
         stream.Free;

         AddLog('Saved '+dataFileName);

       end;
       //PSScript.ExecuteFunction([],'export')
    end else begin
      AddLog('Script error: ' + spritePacker.outputDataFileFormat+'.psc');
      AddLog(PSScript.CompilerErrorToStr(0));
      publishError := true;
    end;
  except
    on E: Exception do begin
      AddLog(E.Message);
      publishError := true;
    end;
  end;

end;
{******************************************************************************}
procedure TMainForm.publishPicture;
  var
    i, j:integer;
    _width,_height, margin, nodeWidth, nodeHeight:Integer;
    node:TSpritePackerNode;
    publishPic:TSSPicture;
    filenameAddon:AnsiString;
    imageFileName:AnsiString;
begin
  try
    publishPic := nil;

    _width:=spritePacker.maxWidth;
    _height:=spritePacker.maxHeight;

    //
    if spritePacker.clampWidthAndHeight = 0 then begin
      //
      _width := 0;
      _height := 0;
      margin := 0;
      if ( spritePacker.postProcess > 0 ) or ( spritePacker.spritesMargin > 0 ) then margin:=1;
      for i:=0 to spritePacker.Count-1 do begin
        node:=spritePacker[i];

        if spritePacker.spritesOptimization > 0 then begin
          nodeWidth:=node.trimMaxX - node.trimMinX + 1;
          nodeHeight:=node.trimMaxY - node.trimMinY + 1;
        end else begin
          nodeWidth:=node.width;
          nodeHeight:=node.height;
        end;

        if ( node.page = currentPage ) then begin
          if ( node.turn ) then begin
            if ( _width < node.x + nodeHeight + margin ) then _width := node.x + nodeHeight + margin;
            if ( _height < node.y + nodeWidth + margin ) then _height := node.y + nodeWidth + margin;
          end else begin
            if ( _width < node.x + nodeWidth + margin ) then _width := node.x + nodeWidth + margin;
            if ( _height < node.y + nodeHeight + margin ) then _height := node.y + nodeHeight + margin;
          end;
        end;
      end;

    end else if spritePacker.clampWidthAndHeight = 1 then begin
      //
      _width := 0;
      _height := 0;
      margin := 0;
      if ( spritePacker.postProcess > 0 ) or ( spritePacker.spritesMargin > 0 ) then margin:=1;
      for i:=0 to spritePacker.Count-1 do begin
        node:=spritePacker[i];

        if spritePacker.spritesOptimization > 0 then begin
          nodeWidth:=node.trimMaxX - node.trimMinX + 1;
          nodeHeight:=node.trimMaxY - node.trimMinY + 1;
        end else begin
          nodeWidth:=node.width;
          nodeHeight:=node.height;
        end;

        if ( node.page = currentPage ) then begin
          if ( node.turn ) then begin
            if ( _width < node.x + nodeHeight + margin ) then _width := node.x + nodeHeight + margin;
            if ( _height < node.y + nodeWidth + margin ) then _height := node.y + nodeWidth + margin;
          end else begin
            if ( _width < node.x + nodeWidth + margin ) then _width := node.x + nodeWidth + margin;
            if ( _height < node.y + nodeHeight + margin ) then _height := node.y + nodeHeight + margin;
          end;
        end;
      end;

      //
      _width:=PowerOfTwo(_width);
      _height:=PowerOfTwo(_height);

    end;

    if ( _width <= 0 ) then _width := spritePacker.maxWidth;
    if ( _height <= 0 ) then _height := spritePacker.maxHeight;

    spritePacker.currentPageClampedWidth := _width;
    spritePacker.currentPageClampedHeight := _height;
    //
    if ( publishPic = nil ) then begin
      publishPic:=TSSPicture.Create;
    end;
    publishPic.SetSize(_width,_height);
    publishPic.bitmap.Canvas.Fill($00000000);

    for i:=0 to spritePacker.Count-1 do begin
      node:=spritePacker[i];
      if ( node.page = currentPage ) then begin
        if ( node.bitmap <> nil ) then begin
          if ( spritePacker.spritesOptimization > 0 ) then begin
            if ( node.turn ) then begin
              publishPic.bitmap.DrawSpecialRect(node.x, node.y, node.bitmap, node.trimMinX, node.trimMinY, node.trimMaxX - node.trimMinX + 1, node.trimMaxY - node.trimMinY + 1, node.turn);
            end else begin
              publishPic.bitmap.DrawSpecialRect(node.x, node.y, node.bitmap, node.trimMinX, node.trimMinY, node.trimMaxX - node.trimMinX + 1, node.trimMaxY - node.trimMinY + 1, node.turn);
            end;
          end else begin
            publishPic.bitmap.DrawSpecial(node.x,node.y,node.bitmap, node.turn);
          end;
        end;
      end;
    end;

    // Post process
    if ( spritePacker.postProcess > 0 ) then begin
      FixHoles(publishPic.bitmap);
      // alpha fix
      for i:=0 to spritePacker.Count-1 do begin
        node:=spritePacker[i];
        if ( node.page = currentPage ) then begin
          if ( node.bitmap <> nil ) then begin
            if ( spritePacker.spritesOptimization > 0 ) then begin
              if ( node.turn ) then begin
                FixHolesAlpha(publishPic.bitmap, node.x, node.y, node.trimMinX, node.trimMinY, node.trimMaxX - node.trimMinX + 1, node.trimMaxY - node.trimMinY + 1, node.turn);
              end else begin
                FixHolesAlpha(publishPic.bitmap, node.x, node.y, node.trimMinX, node.trimMinY, node.trimMaxX - node.trimMinX + 1, node.trimMaxY - node.trimMinY + 1, node.turn);
              end;
            end else begin
              FixHolesAlpha(publishPic.bitmap, node.x,node.y, 0, 0, node.bitmap.Width, node.bitmap.Height, node.turn);
            end;
          end;
        end;
      end;
    end;
    filenameAddon := '';
    if ( currentPage > 0 ) then filenameAddon:='_'+IntToStr(currentPage);

    imageFileName:=spritePacker.outputImageFileName+filenameAddon+'.'+spritePacker.outputImageFileFormat;
    publishPic.bitmap.SaveToFileNew(imageFileName);
    AddLog('Saved '+imageFileName);

    publishPic.Free;
    publishPic := nil;
  except
    on E: Exception do begin
      publishError := true;
      AddLog(E.Message);
    end;
  end;

end;
{******************************************************************************}
end.
{******************************************************************************}
