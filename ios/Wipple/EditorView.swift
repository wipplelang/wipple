import SwiftUI
import WebKit
import Telegraph

class EditorWebViewProxy: ObservableObject {
    fileprivate var webView: WKWebView!
    
    func reload() {
        self.webView.reload()
    }
}

extension EnvironmentValues {
    private struct EditorWebViewProxyEnvironmentKey: EnvironmentKey {
        static var defaultValue: EditorWebViewProxy? = nil
    }
    
    var editorWebViewProxy: EditorWebViewProxy? {
        get { self[EditorWebViewProxyEnvironmentKey.self] }
        set { self[EditorWebViewProxyEnvironmentKey.self] = newValue }
    }
}


struct EditorWebViewReader<Content: View>: View {
    @StateObject private var proxy = EditorWebViewProxy()
    @ViewBuilder var content: (EditorWebViewProxy) -> Content
    
    var body: some View {
        self.content(self.proxy)
            .environment(\.editorWebViewProxy, self.proxy)
    }
}

struct EditorWebView: UIViewRepresentable {
    class RequestHandler: HTTPRequestHandler {
        func respond(to request: HTTPRequest, nextHandler: (HTTPRequest) throws -> HTTPResponse?) throws -> HTTPResponse? {
            guard let response = try nextHandler(request) else { return nil }
            
            if request.method == .GET {
                response.headers["Access-Control-Allow-Origin"] = "*"
                response.headers["Cross-Origin-Opener-Policy"] = "same-origin"
                response.headers["Cross-Origin-Embedder-Policy"] = "require-corp"
            }
            
            return response
        }
    }
    
    class NavigationDelegate: NSObject, WKNavigationDelegate {
        // TODO
    }
    
    class UIDelegate: NSObject, WKUIDelegate {
        let onPresentAlert: (_ message: String, _ completion: @escaping () -> Void) -> Void
        let onPresentPrompt: (_ message: String, _ initialText: String, _ completion: @escaping (String?) -> Void) -> Void
        
        init(onPresentAlert: @escaping (_: String, _: @escaping () -> Void) -> Void, onPresentPrompt: @escaping (_ message: String, _ initialText: String, _ completion: @escaping (String?) -> Void) -> Void) {
            self.onPresentAlert = onPresentAlert
            self.onPresentPrompt = onPresentPrompt
        }
        
        func webView(_ webView: WKWebView, runJavaScriptAlertPanelWithMessage message: String, initiatedByFrame frame: WKFrameInfo, completionHandler: @escaping () -> Void) {
            self.onPresentAlert(message, completionHandler)
        }
        
        func webView(_ webView: WKWebView, runJavaScriptTextInputPanelWithPrompt prompt: String, defaultText: String?, initiatedByFrame frame: WKFrameInfo, completionHandler: @escaping (String?) -> Void) {
            self.onPresentPrompt(prompt, defaultText ?? "", completionHandler)
        }
    }
    
    @Environment(\.editorWebViewProxy) private var proxy
    
    @Binding var document: PlaygroundDocument
    
    var server: Server
    var requestHandler: RequestHandler
    var navigationDelegate: NavigationDelegate
    var uiDelegate: UIDelegate
    var reload: (() -> Void)!
    
    init(document: Binding<PlaygroundDocument>, onPresentAlert: @escaping (_ message: String, _ completion: @escaping () -> Void) -> Void, onPresentPrompt: @escaping (_ message: String, _ initialText: String, _ completion: @escaping (String?) -> Void) -> Void) {
        self._document = document
        
        self.server = Server()
        self.requestHandler = RequestHandler()
        self.navigationDelegate = NavigationDelegate()
        self.uiDelegate = UIDelegate(onPresentAlert: onPresentAlert, onPresentPrompt: onPresentPrompt)
        
        server.httpConfig.requestHandlers.insert(self.requestHandler, at: 0)
        server.serveDirectory(Bundle.main.url(forResource: "dist", withExtension: nil)!, "/playground")
        try! self.server.start()
        
        self.setUp()
    }
    
    func makeUIView(context: Context) -> WKWebView {
        let webView = WKWebView()
        webView.isInspectable = true
        webView.allowsLinkPreview = false
        webView.navigationDelegate = self.navigationDelegate
        webView.uiDelegate = self.uiDelegate
        
        let request = URLRequest(url: URL(string: "http://localhost:\(self.server.port)/playground")!)
        webView.load(request)
        
        self.proxy?.webView = webView
        
        return webView
    }
    
    func updateUIView(_ webView: WKWebView, context: Context) {
        // Do nothing
    }
    
    private func setUp() {
        server.route(.GET, "/.bridge/playground") { request in
            HTTPResponse(
                headers: ["Content-Type": "application/json"],
                content: self.document.json
            )
        }
        
        server.route(.POST, ".bridge/updatePlayground") { request in
            guard let json = String(data: request.body, encoding: .utf8) else {
                return HTTPResponse(.badRequest)
            }
            
            DispatchQueue.main.async {
                self.document.json = json
            }
            
            return HTTPResponse(.ok)
        }
    }
}

struct EditorView: View {
    @Binding var document: PlaygroundDocument
    
    @State private var showAlert = false
    @State private var alertMessage = ""
    @State private var alertAction = AnyView(EmptyView())
    
    @State private var showJSON = false
    @State private var showLessonPicker = false
    
    var body: some View {
        EditorWebViewReader { editorWebView in
            EditorWebView(
                document: self.$document,
                onPresentAlert: { message, completion in
                    self.alertMessage = message
                    self.alertAction = AnyView(Button("OK") { completion() })
                    self.showAlert = true
                },
                onPresentPrompt: { message, initialText, completion in
                    self.alertMessage = message
                    self.alertAction = AnyView(Prompt(initialText: initialText, onSubmit: completion))
                    self.showAlert = true
                }
            )
            .toolbar {
                ToolbarItem(placement: .topBarTrailing) {
                    Menu {
                        Button {
                            editorWebView.reload()
                        } label: {
                            Label("Reload", systemImage: "arrow.counterclockwise")
                        }
                        
                        Button {
                            self.showJSON = true
                        } label: {
                            Label("Export JSON", systemImage: "curlybraces.square")
                        }
                        
                        if #unavailable(iOS 18.0) {
                            Button {
                                self.showLessonPicker = true
                            } label: {
                                Label("Browse Lessons", systemImage: "graduationcap")
                            }
                        }
                    } label: {
                        Label("Options", systemImage: "ellipsis")
                    }
                }
            }
            .sheet(isPresented: self.$showLessonPicker) {
                LessonPicker { document in
                    if let document {
                        self.document = document
                        
                        DispatchQueue.main.async {
                            editorWebView.reload()
                        }
                    }
                    
                    self.showLessonPicker = false
                }
            }
        }
        .onChange(of: self.showAlert) { _, showAlert in
            if !showAlert {
                self.alertMessage = ""
                self.alertAction = AnyView(EmptyView())
            }
        }
        .alert("Wipple", isPresented: self.$showAlert) {
            self.alertAction
        } message: {
            Text(self.alertMessage)
        }
        .sheet(isPresented: self.$showJSON) {
            NavigationStack {
                ScrollView {
                    Text(self.document.json)
                        .font(.body.monospaced())
                        .textSelection(.enabled)
                }
                .navigationTitle("JSON")
                .navigationBarTitleDisplayMode(.inline)
                .navigationBarBackButtonHidden()
                .toolbar {
                    Button("Done") {
                        self.showJSON = false
                    }
                }
            }
        }
    }
}

private struct Prompt: View {
    let onSubmit: (String?) -> Void
    
    @State private var text: String
    
    init(initialText: String, onSubmit: @escaping (String?) -> Void) {
        self.onSubmit = onSubmit
        self._text = State(initialValue: initialText)
    }
    
    var body: some View {
        TextField("", text: self.$text)
        
        Button("Done") {
            self.onSubmit(self.text)
        }
        
        Button("Cancel", role: .cancel) {
            self.onSubmit(nil)
        }
    }
}
