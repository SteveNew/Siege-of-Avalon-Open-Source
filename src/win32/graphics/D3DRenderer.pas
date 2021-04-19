unit D3DRenderer;

interface

uses
  SysUtils, Classes, SyncObjs, System.Generics.Collections,
  Winapi.Windows, Winapi.Messages,
  Winapi.D3D11, Winapi.DXGI, Winapi.D3DCommon, Winapi.DXTypes, Winapi.DXGIFormat, Winapi.DXGIType,
  D3DShader, D3DMesh;

const
  dxfmt_565: Integer = 0;
  dxfmt_r16: Integer = 1;
  dxfmt_8888: Integer = 2;
  blend_none: Integer = 0;
  blend_transparent: Integer = 1;

type
  TDXRenderLayer = class
  private
    FDevice: ID3D11Device;
    FDeviceContext : ID3D11DeviceContext;
    FShader: TDXTextureShader;
    FBlendState: ID3D11BlendState;
    FTexture: ID3D11Texture2D;
    FTextureSRV: ID3D11ShaderResourceView;
    FEnabled: Boolean;
    FSize, FViewportSize: TSize;

    FSourceRect, FDestRect: TRect;

    FLock: TCriticalSection;
  public
    Constructor Create(DeviceContext: ID3D11DeviceContext; aWidth, aHeight: Integer; aFormat, aBlend: Integer);
    Destructor Destroy; override;

    Procedure UpdateTexture(Data: Pointer; Stride: Cardinal); overload;
    Procedure UpdateTexture(Data: Pointer; Stride: Cardinal; Rect: TRect); overload;
    procedure Activate;
    procedure SetSourceRect(aRect: TRect);
    procedure SetDestRect(aRect: TRect);

    property Enabled: Boolean read FEnabled write FEnabled;

    procedure Lock;
    procedure Unlock;
  end;

  TDXPresenterThread = class;

  TDXRenderer = class
    private
      FDevice: ID3D11Device;
      FDeviceContext: ID3D11DeviceContext;
      FCurrentFeatureLevel: TD3D_FEATURE_LEVEL;

      FSwapchain: IDXGISwapChain;
      FRenderTargetView: ID3D11RenderTargetView;

      FRasterizerState: ID3D11RasterizerState;
      FViewport: TD3D11_VIEWPORT;

      FReady, FEnableVSync: Boolean;

      FQuad: TDXModel;
      FLayers: TList<TDXRenderLayer>;
      FMainLayer: TDXRenderLayer;

      FThread: TDXPresenterThread;
      FQuitThread: Boolean;

      Function Initialize(aHWND: HWND; aWidth, aHeight: Integer; bWindowed, bVSync: Boolean): HRESULT;
      Function Uninitialize: HRESULT;
      Function InitializeTexture(aWidth, aHeight: Integer): HRESULT;
    public
      Constructor Create(aHWND: HWND; aWidth, aHeight: Integer; bWindowed, bVSync: Boolean);
      Destructor Destroy; override;

      function CreateLayer(aWidth, aHeight: Integer; Format, Blend: Integer): TDXRenderLayer;

      Procedure UpdateTexture(Data: Pointer; Stride: Cardinal); overload;
      Procedure UpdateTexture(Data: Pointer; Stride: Cardinal; Rect: TRect); overload;
      Function Clear(aColor: TFourSingleArray): HRESULT;
      Function Render: HRESULT;
      Function Present: HRESULT;

      procedure EnableFullscreen(Enabled: Boolean);

      procedure StartPresenterThread;
      procedure StopPresenterThread;
  end;

  TDXPresenterThread = class(TThread)
    private
      FRenderer: TDXRenderer;
      FQuit: Boolean;
    public
      constructor Create(aRenderer: TDXRenderer);
      destructor Destroy; override;
      procedure Execute; override;
      procedure Stop;
  end;

implementation

uses IOUtils, LogFile, Winapi.D3DX10;

var
  vertex_shader: string = 'cbuffer MatrixBuffer'#13#10 +
    '{'#13#10 +
    '    matrix OutputPosition;'#13#10 +
    '    matrix InputPosition;'#13#10 +
    '}'#13#10 +
    'struct VSInput'#13#10 +
    '{'#13#10 +
    '    float4 position : POSITION;'#13#10 +
    '    float2 texcoords : TEXCOORD0;'#13#10 +
    '};'#13#10 +
    ''#13#10 +
    'struct PSInput'#13#10 +
    '{'#13#10 +
    '    float4 position : SV_POSITION;'#13#10 +
    '    float2 texcoords : TEXCOORD0;'#13#10 +
    '};'#13#10 +
    ''#13#10 +
    'PSInput VSEntry(VSInput input)'#13#10 +
    '{'#13#10 +
    '    PSInput output;'#13#10 +
    '    input.position.w = 1.0f;'#13#10 +
    '    output.position = mul(input.position, OutputPosition);'#13#10 +
    '    float4 tex = float4(input.texcoords.x, input.texcoords.y, 0.0f, 1.0f);'#13#10 +
    '    tex = mul(tex, InputPosition);'#13#10 +
    '    output.texcoords = float2(tex.x, 1.0f - tex.y);'#13#10 +
    '    return output;'#13#10 +
    '}'#13#10;

  fragment_shader: string = 'Texture2D DiffuseMap;'#13#10 +
    'SamplerState SampleType;'#13#10 +
    ''#13#10 +
    'struct PSInput'#13#10 +
    '{'#13#10 +
    '    float4 position : SV_POSITION;'#13#10 +
    '    float2 texcoords : TEXCOORD0;'#13#10 +
    '};'#13#10 +
    ''#13#10 +
    'float4 PSEntry(PSInput vs_out) : SV_TARGET'#13#10 +
    '{'#13#10 +
    '	float4 color = DiffuseMap.Sample(SampleType, vs_out.texcoords); '#13#10 +
//    ' if (color.a < 0.1f) discard;'#13#10 +
    '	return color.rgba; '#13#10 +
    '}'#13#10;

  fragment_shader_R16_bitwise: string = 'Texture2D DiffuseMap;'#13#10 +
    'SamplerState SampleType;'#13#10 +
    ''#13#10 +
    'struct PSInput'#13#10 +
    '{'#13#10 +
    '    float4 position : SV_POSITION;'#13#10 +
    '    float2 texcoords : TEXCOORD0;'#13#10 +
    '};'#13#10 +
    ''#13#10 +
    'float4 PSEntry(PSInput vs_out) : SV_TARGET'#13#10 +
    '{'#13#10 +
    '    float4 c = DiffuseMap.Sample(SampleType, vs_out.texcoords);'#13#10 +
    '    int c565 = c.r * 65535;'#13#10 +
    '    int r = (c565 & 0xF800) >> 11;'#13#10 +
    '    int g = (c565 & 0x7E0) >> 5;'#13#10 +
    '    int b = c565 & 0x1F;'#13#10 +
    '	return float4(r / 32.0f, g / 64.0f, b / 32.0f, 1.0f);'#13#10 +
    '}'#13#10;

  fragment_shader_R16_int: string = 'Texture2D DiffuseMap;'#13#10 +
    'SamplerState SampleType;'#13#10 +
    ''#13#10 +
    'struct PSInput'#13#10 +
    '{'#13#10 +
    '    float4 position : SV_POSITION;'#13#10 +
    '    float2 texcoords : TEXCOORD0;'#13#10 +
    '};'#13#10 +
    ''#13#10 +
    'float4 PSEntry(PSInput vs_out) : SV_TARGET'#13#10 +
    '{'#13#10 +
    '    float4 c = DiffuseMap.Sample(SampleType, vs_out.texcoords);'#13#10 +
    '    int c565 = c.r * 65535;'#13#10 +
    '    int t = c565 / 32;'#13#10 +
    '    t = t * 32;'#13#10 +
    '    int b = c565 - t;'#13#10 +
    '    c565 = (c565 - b) / 32;'#13#10 +
    '    t = c565 / 64;'#13#10 +
    '    t = t * 64;'#13#10 +
    '    int g = c565 - t;'#13#10 +
    '    c565 = (c565 - g) / 64;'#13#10 +
    '    t = c565 / 32;'#13#10 +
    '    t = t * 32;'#13#10 +
    '    int r = c565 - t;'#13#10 +
    '	return float4(r / 32.0f, g / 64.0f, b / 32.0f, 1.0f);'#13#10 +
    '}'#13#10;

function TDXRenderer.Initialize(aHWND: HWND; aWidth, aHeight: Integer; bWindowed, bVSync: Boolean): HRESULT;
var
  feature_level: Array[0..0] of TD3D_FEATURE_LEVEL;
  pBackbuffer: ID3D11Texture2D;

  swapchain_desc: DXGI_SWAP_CHAIN_DESC;
  rast_state_desc: TD3D11_RASTERIZER_DESC;

  dxgidev: IDXGIDevice;
  dxgiadapter: IDXGIAdapter;
  dxgifactory: IDXGIFactory;
begin
  If FReady then Begin
    Result := Uninitialize;
    If Failed(Result) then Exit;
  end;

  FLayers := TList<TDXRenderLayer>.Create;

  ZeroMemory(@swapchain_desc, sizeof(swapchain_desc));
  With swapchain_desc do Begin
    BufferCount := 2;

    BufferDesc.Width := aWidth;
    BufferDesc.Height := aHeight;
    BufferDesc.Format := DXGI_FORMAT_R8G8B8A8_UNORM;
    BufferDesc.RefreshRate.Numerator := 144;
    BufferDesc.RefreshRate.Denominator := 1;
    BufferDesc.ScanlineOrdering := DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED;
    BufferDesc.Scaling := DXGI_MODE_SCALING_UNSPECIFIED;
    BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
    OutputWindow := aHWND;
    SampleDesc.Count := 1;
    SampleDesc.Quality := 0;
    Windowed := bWindowed;
    SwapEffect := DXGI_SWAP_EFFECT_SEQUENTIAL;
    Flags := 0;
  End;

  feature_level[0] := D3D_FEATURE_LEVEL_10_0;

  Result := D3D11CreateDeviceAndSwapChain(
      nil,
      D3D_DRIVER_TYPE_HARDWARE,
      0,
      0,
      @feature_level[0],
      1,
      D3D11_SDK_VERSION,
      @swapchain_desc,
      FSwapchain,
      FDevice,
      FCurrentFeatureLevel,
      FDeviceContext
  );
  If Failed(Result) then
  begin
    Log.Log('D3D', 'Failed to create device and swapchain (%.8X)', [Result]);
  end;

  Result := FSwapchain.GetBuffer(0, ID3D11Texture2D, pBackbuffer);
  If Failed(Result) then
  begin
    Log.Log('D3D', 'Failed get back buffer (%.8X)', [Result]);
  end;


  Result := FDevice.CreateRenderTargetView(pBackbuffer, nil, FRenderTargetView);
  If Failed(Result) then
  begin
    Log.Log('D3D', 'Failed to create render target view (%.8X)', [Result]);
  end;

  pBackbuffer := nil;

  FDeviceContext.OMSetRenderTargets(1, FRenderTargetView, nil);

  ZeroMemory(@rast_state_desc, sizeof(rast_state_desc));
  With rast_state_desc do Begin
    AntialiasedLineEnable := True;
    CullMode := D3D11_CULL_NONE;
    DepthBias := 0;
    DepthBiasClamp := 0;
    DepthClipEnable := True;
    FillMode := D3D11_FILL_SOLID;
    FrontCounterClockwise := False;
    MultisampleEnable := False;
    ScissorEnable := False;
    SlopeScaledDepthBias := 0;
  End;

  Result := FDevice.CreateRasterizerState(rast_state_desc, FRasterizerState);
  If Failed(Result) then
  begin
    Log.Log('D3D', 'Failed to create rasterizer state (%.8X)', [Result]);
  end;

  FDeviceContext.RSSetState(FRasterizerState);

  ZeroMemory(@FViewport, sizeof(FViewport));
  With FViewport do Begin
    Width := aWidth;
    Height := aHeight;
    MinDepth := 0;
    MaxDepth := 1;
    TopLeftX := 0;
    TopLeftY := 0;
  End;

  FDeviceContext.RSSetViewports(1, @FViewport);
  FQuad := TDXModel.CreateQuad(FDeviceContext);

  (*
  if FileExists('shaders\default.vs') then
  begin
    vertex_shader := TFile.ReadAllText('shaders\default.vs');
  end;

  if FileExists('shaders\default.ps') then
  begin
    fragment_shader := TFile.ReadAllText('shaders\default.ps');
  end;
  *)

  InitializeTexture(aWidth, aHeight);

  FDevice.QueryInterface(IDXGIDevice, dxgidev);
  if Assigned(dxgidev) then
  begin
    dxgidev.GetParent(IDXGIAdapter, dxgiadapter);
    if Assigned(dxgiadapter) then
    begin
      dxgiadapter.GetParent(IDXGIFactory, dxgifactory);
      if Assigned(dxgifactory) then
      begin
        dxgifactory.MakeWindowAssociation(aHWND, DXGI_MWA_NO_ALT_ENTER or DXGI_MWA_NO_WINDOW_CHANGES);
        dxgifactory := nil;
      end;
      dxgiadapter := nil;
    end;
    dxgidev := nil;
  end;
  FReady := True;
end;

function TDXRenderer.Uninitialize: HRESULT;
var
  Layer: TDXRenderLayer;
begin
  If not FReady then
     Exit(E_FAIL);

  for Layer in FLayers do
  begin
    Layer.Free;
  end;

  FLayers.Clear;

  FSwapchain.SetFullscreenState(FALSE, nil);

  FQuad.Free;

  FRasterizerState := nil;
  FRenderTargetView := nil;
  FDeviceContext := nil;
  FDevice := nil;

  FSwapchain := nil;

  FReady := False;

  Result := S_OK;
end;

function TDXRenderer.InitializeTexture(aWidth, aHeight: Integer): HRESULT;
begin
  FMainLayer := CreateLayer(aWidth, aHeight, dxfmt_r16, blend_none);
end;

procedure TDXRenderer.UpdateTexture(Data: Pointer; Stride: Cardinal);
begin
  FMainLayer.UpdateTexture(Data, Stride);
end;

procedure TDXRenderer.UpdateTexture(Data: Pointer; Stride: Cardinal; Rect: TRect);
begin
  FMainLayer.UpdateTexture(Data, Stride, Rect);
end;

constructor TDXRenderer.Create(aHWND: HWND; aWidth, aHeight: Integer; bWindowed, bVSync: Boolean);
begin
  Inherited Create;

  FReady := False;
  FEnableVSync := bVSync;

  If Failed(Initialize(aHWND, aWidth, aHeight, bWindowed, bVSync)) then
     Raise Exception.Create('Direct3D 11 initialization failed');
end;

function TDXRenderer.CreateLayer(aWidth, aHeight,
  Format, Blend: Integer): TDXRenderLayer;
begin
  Result := TDXRenderLayer.Create(FDeviceContext, aWidth, aHeight, Format, Blend);
  FLayers.Add(Result);
end;

destructor TDXRenderer.Destroy;
begin
  StopPresenterThread;
  Uninitialize;
  Inherited;
end;

procedure TDXRenderer.EnableFullscreen(Enabled: Boolean);
var
  dxgioutput: IDXGIOutput;
begin
  if Assigned(FSwapchain) then
  begin
    FSwapchain.GetContainingOutput(dxgioutput);
    if Assigned(dxgioutput) then
    begin
      FSwapchain.SetFullscreenState(Enabled, dxgioutput);
      dxgioutput := nil;
    end;
  end;
//  FSwapchain.SetFullscreenState(Enabled, )
end;

function TDXRenderer.Clear(aColor: TFourSingleArray): HRESULT;
begin
  If not FReady then Begin
    Result := E_FAIL;
    Exit;
  end;
  FDeviceContext.ClearRenderTargetView(FRenderTargetView, aColor);
  Result := S_OK;
end;

function TDXRenderer.Render: HRESULT;
var
  Layer: TDXRenderLayer;
begin
  If not FReady then Begin
    Result := E_FAIL;
    Exit;
  End;
  for Layer in FLayers do
  begin
    if Layer.Enabled then
    begin
      Layer.Lock;
      try
        Layer.Activate;
        FQuad.Render(FDeviceContext);
      finally
        Layer.Unlock;
      end;
    end;
  end;
  Result := 0;//
end;

//TThreadProc

procedure TDXRenderer.StartPresenterThread;
begin
  if not Assigned(FThread) then
  begin
    FThread := TDXPresenterThread.Create(Self);
    FThread.Start;
  end;
end;

procedure TDXRenderer.StopPresenterThread;
begin
  if Assigned(FThread) then
  begin
    FThread.Stop;
    FThread.Destroy;
  end;
end;

function TDXRenderer.Present: HRESULT;
begin
  If not FReady then Begin
    Result := E_FAIL;
    Exit;
  End;

  If FEnableVSync then Begin
    FSwapchain.Present(1, 0);
  end else Begin
    FSwapchain.Present(0, 0);
  end;

  Result := S_OK;
end;

{ TDXRenderLayer }

constructor TDXRenderLayer.Create(DeviceContext: ID3D11DeviceContext; aWidth,
  aHeight, aFormat, aBlend: Integer);
var
  desc: TD3D11_TEXTURE2D_DESC;
  srv_desc: TD3D11_SHADER_RESOURCE_VIEW_DESC;
  blend_desc: TD3D11_BLEND_DESC;
  Res: HRESULT;
  NumVP: Cardinal;
  VP: TD3D11_VIEWPORT;

  FragShader: String;
  PixelFormat: DXGI_FORMAT;
begin
  FLock := TCriticalSection.Create;

  FSize.Width := aWidth;
  FSize.Height := aHeight;

  FEnabled := True;
  FDeviceContext := DeviceContext;
  FDeviceContext.GetDevice(FDevice);
  NumVP := 1;
  FDeviceContext.RSGetViewports(NumVP, @VP);
  FViewportSize.Width := Trunc(VP.Width);
  FViewportSize.Height := Trunc(VP.Height);

  FBlendState := nil;
  blend_desc := TD3D11_BLEND_DESC.Create(True);
  if aBlend = blend_transparent then
  begin
    blend_desc.RenderTarget[0].BlendEnable := True;
    blend_desc.RenderTarget[0].SrcBlend := D3D11_BLEND_SRC_ALPHA;
    blend_desc.RenderTarget[0].DestBlend := D3D11_BLEND_INV_SRC_ALPHA;
    blend_desc.RenderTarget[0].BlendOp := D3D11_BLEND_OP_ADD;
    blend_desc.RenderTarget[0].SrcBlendAlpha := D3D11_BLEND_ZERO;
    blend_desc.RenderTarget[0].DestBlendAlpha := D3D11_BLEND_ZERO;
    blend_desc.RenderTarget[0].BlendOpAlpha := D3D11_BLEND_OP_ADD;
    blend_desc.RenderTarget[0].RenderTargetWriteMask := Byte(D3D11_COLOR_WRITE_ENABLE_ALL);
    Res := FDevice.CreateBlendState(blend_desc, FBlendState);
  end;

  if aFormat = dxfmt_565 then
  begin
    PixelFormat := DXGI_FORMAT_B5G6R5_UNORM;
    FragShader := fragment_shader;
  end
  else if aFormat = dxfmt_r16 then
  begin
    PixelFormat := DXGI_FORMAT_R16_UNORM;
    FragShader := fragment_shader_R16_int;
  end
  else if aFormat = dxfmt_8888 then
  begin
    PixelFormat := DXGI_FORMAT_R8G8B8A8_UNORM;
    FragShader := fragment_shader;
  end;

  With desc do Begin
    Width := aWidth;
    Height := aHeight;
    MipLevels := 0;
    ArraySize := 1;
    Format := PixelFormat;
    SampleDesc.Count := 1;
    SampleDesc.Quality := 0;
    Usage := D3D11_USAGE_DEFAULT;
    BindFlags := Ord(D3D11_BIND_SHADER_RESOURCE) or Ord(D3D11_BIND_RENDER_TARGET);
    CPUAccessFlags := 0;
    MiscFlags := Ord(D3D11_RESOURCE_MISC_GENERATE_MIPS);
  End;

  FDevice.CreateTexture2D(desc, nil, FTexture);

  With srv_desc do Begin
    Format := desc.Format;
    ViewDimension := D3D11_SRV_DIMENSION_TEXTURE2D;
    Texture2D.MostDetailedMip := 0;
    Texture2D.MipLevels := 1;
  End;

  FDevice.CreateShaderResourceView(FTexture, @srv_desc, FTextureSRV);

  FShader := TDXTextureShader.Create(FDevice, vertex_shader, FragShader);

  Res := FShader.SetTexture(FDeviceContext, FTextureSRV);

  If Failed(Res) then
  begin
    Log.Log('D3D', 'Failed to set texture to shader (%.8X)', [Res]);
  end;

//  Res := FShader.Activate(FDeviceContext);
//  If Failed(Res) then
//  begin
//    Log.Log('D3D', 'Failed to activate shader (%.8X)', [Res]);
//  end;

end;

destructor TDXRenderLayer.Destroy;
begin
  FShader.Free;
  FTexture := nil;
  FTextureSRV := nil;
  FBlendState := nil;
  FLock.Free;
  inherited;
end;

procedure TDXRenderLayer.Lock;
begin
  FLock.Enter;
end;

procedure TDXRenderLayer.SetDestRect(aRect: TRect);
var
  l, t, r, b: Single;
  T1, S, T2, Temp, RR: TD3DMatrix;
  H: Integer;
begin
  H := aRect.Top;
  aRect.Top := FViewportSize.Height - aRect.Bottom;
  aRect.Bottom := FViewportSize.Height - H;
  l := aRect.Left/FViewportSize.Width*2 - 1.0;
  t := aRect.Top/FViewportSize.Height*2 - 1.0;
  r := aRect.Right/FViewportSize.Width*2 - 1.0;
  b := aRect.Bottom/FViewportSize.Height*2 - 1.0;

  D3DXMatrixTranslation(T1, l, t, 0);
  D3DXMatrixScaling(S, (r - l)/2, (b - t)/2, 1.0);
  D3DXMatrixTranslation(T2, 1, 1, 0);
  D3DXMatrixMultiply(Temp, T2, S);
  D3DXMatrixMultiplyTranspose(RR, Temp, T1);
  Lock;
  FShader.OutputTransform := RR;
  Unlock;
end;

procedure TDXRenderLayer.SetSourceRect(aRect: TRect);
var
  T1, S, T2, Temp, R: TD3DMatrix;
begin
  D3DXMatrixTranslation(T1, aRect.Left/FSize.Width, aRect.Top/FSize.Height, 0);
  D3DXMatrixScaling(S, aRect.Width/FSize.Width, aRect.Height/FSize.Height, 1.0);
  D3DXMatrixMultiplyTranspose(Temp, S, T1);
  Lock;
  FShader.InputTransform := Temp;
  Unlock;
end;

procedure TDXRenderLayer.Activate;
var
  f: TFourSingleArray;
begin
  f[0] := 0;
  f[1] := 0;
  f[2] := 0;
  f[3] := 0;
  FDeviceContext.OMSetBlendState(FBlendState, f, $FFFFFFFF);
  FShader.SetTexture(FDeviceContext, FTextureSRV);
  FShader.Activate(FDeviceContext);
end;

procedure TDXRenderLayer.UpdateTexture(Data: Pointer; Stride: Cardinal);
begin
  Lock;
  try
    FDeviceContext.UpdateSubresource(FTexture, 0, nil, data, stride, 0);
  finally
    Unlock;
  end;
end;

procedure TDXRenderLayer.Unlock;
begin
  FLock.Leave;
end;

procedure TDXRenderLayer.UpdateTexture(Data: Pointer; Stride: Cardinal;
  Rect: TRect);
var
  Box: D3D11_BOX;
begin
  Lock;
  try
    Box.left := Rect.Left;
    Box.top := Rect.Top;
    Box.right := Rect.Right;
    Box.bottom := Rect.Bottom;
    Box.front := 0;
    Box.back := 1;
    FDeviceContext.UpdateSubresource(FTexture, 0, @Box, data, stride, 0);
  finally
    Unlock;
  end;
end;

{ TDXPresenterThread }

constructor TDXPresenterThread.Create(aRenderer: TDXRenderer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FRenderer := aRenderer;
  FQuit := False;
end;

destructor TDXPresenterThread.Destroy;
begin
  Stop;
end;

procedure TDXPresenterThread.Execute;
begin
  while not FQuit do
  begin
    FRenderer.Render;
    FRenderer.Present;
    Sleep(1);
  end;
end;

procedure TDXPresenterThread.Stop;
begin
  FQuit := True;
  WaitFor;
end;

end.

