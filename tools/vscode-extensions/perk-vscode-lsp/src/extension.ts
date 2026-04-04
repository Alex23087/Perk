import * as vscode from 'vscode';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { execFileSync } from 'child_process';

export function activate(context: vscode.ExtensionContext) {
    const diagnosticCollection = vscode.languages.createDiagnosticCollection('Perk');

    // debounce timers per-document
    const debounceTimers = new Map<string, NodeJS.Timeout>();

    const handleDocument = (document: vscode.TextDocument) => {
        if (!(document.languageId === 'perk')) return;

        const diagnostics: vscode.Diagnostic[] = [];

        let extraArgs: string[] = [];
        const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
        if (workspaceFolder) {
            const configPath = path.join(workspaceFolder.uri.fsPath, '.perkconf.json');
            try {
                if (fs.existsSync(configPath)) {
                    const raw = fs.readFileSync(configPath, 'utf8');
                    const parsed = JSON.parse(raw);
                    if (Array.isArray(parsed) && parsed.every((opt) => typeof opt === 'string')) {
                        extraArgs = parsed;
                    } else {
                        vscode.window.showWarningMessage('.perkconf.json must be an array of strings.');
                    }
                }
            } catch (error: any) {
                vscode.window.showWarningMessage(`Failed to read .perkconf.json: ${error.message}`);
            }
        }

        // pass undefined for dir when unsaved/untitled
        const dir = document.isUntitled ? undefined : document.uri.fsPath;
        const errors = runCompiler(document.getText(), dir, extraArgs);

        errors.forEach((error) => {
            const startLine = Math.max(0, error.startLine);
            const endLine = Math.max(0, error.endLine);
            const range = new vscode.Range(
                new vscode.Position(startLine, Math.max(0, error.characterStart)),
                new vscode.Position(endLine, Math.max(0, error.characterEnd))
            );
            diagnostics.push(new vscode.Diagnostic(
                range,
                `${(error.errorType.charAt(0).toUpperCase() + error.errorType.slice(1) + " Error")}: ${error.message}`,
                vscode.DiagnosticSeverity.Error
            ));
        });

        diagnosticCollection.set(document.uri, diagnostics.length ? diagnostics : []);
    };

    // debounced change handler
    const changeSub = vscode.workspace.onDidChangeTextDocument((event) => {
        const key = event.document.uri.toString();
        if (debounceTimers.has(key)) {
            clearTimeout(debounceTimers.get(key)!);
        }
        debounceTimers.set(key, setTimeout(() => {
            debounceTimers.delete(key);
            handleDocument(event.document);
        }, 350));
    });

    // also run on open/save so user sees diagnostics quickly
    const openSub = vscode.workspace.onDidOpenTextDocument(handleDocument);
    const saveSub = vscode.workspace.onDidSaveTextDocument(handleDocument);

    context.subscriptions.push(diagnosticCollection, changeSub, openSub, saveSub);
}

export function deactivate() { }

function runCompiler(
    sourceCode: string,
    dir: string | undefined,
    extraArgs: string[] = []
): { errorType: string; startLine: number; characterStart: number; endLine: number; characterEnd: number; message: string }[] {
    const errors: { errorType: string; startLine: number; characterStart: number; endLine: number; characterEnd: number; message: string }[] = [];

    // Create a temporary file to store the source code
    const tmpDir: string = os.tmpdir();
    const tmpFile: string = path.join(tmpDir, dir ? path.basename(dir) : `source-${Date.now()}.tmp.perk`);

    fs.writeFileSync(tmpFile, sourceCode, 'utf8');

    const args: string[] = ['--check', '--json', tmpFile, ...extraArgs];

    if (dir) {
        args.push('--dir', path.dirname(dir));
    }

    // Wait for the command to finish
    try {
        const result = execFileSync('perkc', args, { encoding: 'utf8' });
        if (result !== '') {
            const parsedError = JSON.parse(result);
            errors.push({
                errorType: parsedError.error,
                startLine: parsedError.start_line - 1,
                endLine: parsedError.end_line - 1,
                characterStart: parsedError.start_col - 1,
                characterEnd: parsedError.end_col - 1,
                message: parsedError.message,
            });
        }
    } catch (execError: any) {
        // If the compiler returns an error, print it as a regular error
        errors.push({
            errorType: 'execution',
            startLine: -1,
            endLine: -1,
            characterStart: -1,
            characterEnd: -1,
            message: execError.message,
        });
    }

    // Clean up the temporary file
    try {
        fs.unlinkSync(tmpFile);
    } catch (unlinkError: any) {
        vscode.window.showWarningMessage(`Failed to delete temporary file: ${unlinkError.message}`);
    }

    return errors;
}