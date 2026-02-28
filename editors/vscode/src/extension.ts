import * as path from "path";
import { workspace, ExtensionContext } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const pythonPath =
    workspace.getConfiguration("abapLsp").get<string>("pythonPath") ||
    "python3";

  const serverOptions: ServerOptions = {
    command: pythonPath,
    args: ["-m", "abap_lsp"],
    options: {
      cwd: path.resolve(__dirname, "..", "..", "..", "abap-lsp"),
    },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "abap" }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.abap"),
    },
  };

  client = new LanguageClient(
    "abapLsp",
    "ABAP Language Server",
    serverOptions,
    clientOptions
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
