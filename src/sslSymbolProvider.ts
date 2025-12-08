import * as vscode from "vscode";
import { Lexer, Token, TokenType } from "./parsing/lexer";
import { Parser, Node, NodeType } from "./parsing/parser";

/**
 * SSL Document Symbol Provider
 * Refactored to use the centralized Lexer and Parser.
 */
export class SSLSymbolProvider implements vscode.DocumentSymbolProvider {

	public provideDocumentSymbols(
		document: vscode.TextDocument,
		token: vscode.CancellationToken
	): vscode.DocumentSymbol[] {
		const text = document.getText();
		const lexer = new Lexer(text);
		const tokens = lexer.tokenize();

		if (token && token.isCancellationRequested) {
			return [];
		}

		const parser = new Parser(tokens);
		const root = parser.parse();

		// Root container
		const rootSymbols: vscode.DocumentSymbol[] = [];
		this.processNodes(root.children, rootSymbols);

		return rootSymbols;
	}

	private processNodes(nodes: Node[], parentContainer: vscode.DocumentSymbol[]): void {
		let currentContainer = parentContainer;
		const regionStack: vscode.DocumentSymbol[] = [];

		for (let i = 0; i < nodes.length; i++) {
			const node = nodes[i];

			// 1. Handle Regions (Comment-style /* region */)
			// Note: :REGION blocks are handled below as Statements starting a block
			if (node.type === NodeType.RegionStart) {
				const name = this.extractRegionName(node);
				const sym = this.createSymbol(
					name,
					vscode.SymbolKind.Namespace,
					node.startLine - 1,
					"Region",
					new vscode.Range(node.startLine - 1, 0, node.endLine - 1, 100)
				);
				currentContainer.push(sym);
				regionStack.push(sym);
				currentContainer = sym.children;
				continue;
			}

			if (node.type === NodeType.RegionEnd) {
				if (regionStack.length > 0) {
					const closedRegion = regionStack.pop()!;
					const endLine = node.endLine - 1;
					closedRegion.range = new vscode.Range(closedRegion.range.start, new vscode.Position(endLine, 0));

					if (regionStack.length > 0) {
						currentContainer = regionStack[regionStack.length - 1].children;
					} else {
						currentContainer = parentContainer;
					}
				}
				continue;
			}

			// 2. Handle Statements (Potential Block Starts or Variables)
			if (node.type === NodeType.Statement) {
				const firstToken = this.getFirstSignificantToken(node);
				if (!firstToken) { continue; }

				const type = firstToken.text.toUpperCase().replace(/^:+/, '');

				// Check key block starters
				if (type === "PROCEDURE" || type === "CLASS" || type === "REGION") {
					let symKind = vscode.SymbolKind.Function;
					let symDetail = "Procedure";
					let symName = "";

					if (type === "CLASS") {
						symKind = vscode.SymbolKind.Class;
						symDetail = "Class";
						symName = this.extractNameAfter(node.tokens, "CLASS");
					} else if (type === "REGION") {
						symKind = vscode.SymbolKind.Namespace;
						symDetail = "Region";
						symName = this.extractNameAfter(node.tokens, "REGION");
					} else {
						symName = this.extractNameAfter(node.tokens, "PROCEDURE");
					}

					// Create Symbol
					const sym = this.createSymbol(
						symName,
						symKind,
						node.startLine - 1,
						symDetail,
						// Preliminary range, will update if we find end
						new vscode.Range(node.startLine - 1, 0, node.endLine - 1, 0)
					);

					currentContainer.push(sym);

					// Look ahead for the Block (body)
					// It should be the very next node if the parser logic holds
					if (i + 1 < nodes.length && nodes[i + 1].type === NodeType.Block) {
						const blockNode = nodes[i + 1];
						// Recurse into the block body, adding to THIS symbol's children
						this.processNodes(blockNode.children, sym.children);

						// Update range to cover the block
						// Actually better to cover until the END statement which follows the block?
						// The Block node endLine might not include the END statement?
						// Parser: Block endLine is set to last line of block content usually.
						// Wait, Parser handleBlockEnd sets `popped.endLine = stmt.endLine`.
						// But `popped` matches `newBlock`. So `blockNode.endLine` SHOULD include the end keyword line.
						// Let's trust blockNode.endLine for now.
						sym.range = new vscode.Range(sym.range.start, new vscode.Position(blockNode.endLine - 1, 0)); // Approx col 0

						// Skip the block node in main loop
						i++;
					}
					else {
						// Edge case: Block missing? Or purely empty procedure? 
						// If just :PROCEDURE ... :ENDPROC without content, Parser might still create Block?
						// handleBlockMiddle/End logic always adds Block node?
						// Yes, handleBlockStart creates newBlock and pushes to children.
						// So Block node should exist.
					}
					continue;
				}

				// Variable Declarations
				if (type === "DECLARE" || type === "PARAMETERS") {
					const isParam = type === "PARAMETERS";
					const vars = this.extractVariables(node.tokens, type);

					for (const v of vars) {
						const sym = new vscode.DocumentSymbol(
							v,
							isParam ? "Parameter" : "Variable",
							vscode.SymbolKind.Variable,
							new vscode.Range(node.startLine - 1, 0, node.endLine - 1, 0),
							new vscode.Range(node.startLine - 1, 0, node.endLine - 1, 0)
						);
						currentContainer.push(sym);
					}
				}
			}

			// 3. Handle Generic Blocks (IF, WHILE) that are NOT symbols themselves
			// The Start Statement (:IF) was ignored above.
			// The Block node (body) follows.
			// We must traverse it to find variables inside it.
			if (node.type === NodeType.Block) {
				// Flatten: Add children of IF to currentContainer
				this.processNodes(node.children, currentContainer);
			}
		}
	}

	private createSymbol(name: string, kind: vscode.SymbolKind, line: number, detail: string, range: vscode.Range): vscode.DocumentSymbol {
		const selRange = new vscode.Range(line, 0, line, name.length);
		// Improvement: actual range of the name token would be better, but this is sufficient for now
		return new vscode.DocumentSymbol(name, detail, kind, range, selRange);
	}

	private extractRegionName(node: Node): string {
		// Tokens: /*, region, Name, */
		// We can just grab the text and parse it, or find the token after 'region'
		for (const t of node.tokens) {
			if (t.type === TokenType.Comment) {
				const match = /\/\*\s*region\s+(.+?)(;|$)/i.exec(t.text);
				if (match) {
					return match[1].trim();
				}
			}
		}
		return "Region";
	}

	private extractNameAfter(tokens: Token[], keyword: string): string {
		// Find keyword, take content
		for (let i = 0; i < tokens.length - 1; i++) {
			if (tokens[i].text.toUpperCase().replace(/^:+/, '') === keyword) {
				if (keyword === "REGION") {
					// Capture until semicolon or end
					let nameParts: string[] = [];
					let j = i + 1;
					while (j < tokens.length) {
						const t = tokens[j];
						if (t.text === ';' || t.type === TokenType.Unknown) { // unknown might be newline? No, Lexer handles newlines.
							break;
						}
						if (t.type !== TokenType.Whitespace && t.type !== TokenType.Comment) {
							nameParts.push(t.text);
						} else if (t.type === TokenType.Whitespace && nameParts.length > 0) {
							// Preserve single spaces between words?
							// tokens: Main, Space, Logic.
							// nameParts: Main. Space skipped. Logic. -> MainLogic?
							// Better to capture text directly from pos?
							// Simple approximation: add space if skipping whitespace and previously added part.
							nameParts.push(" ");
						}
						j++;
					}
					return nameParts.join("").trim();
				} else {
					// Existing single identifier logic
					let j = i + 1;
					while (j < tokens.length && tokens[j].type === TokenType.Whitespace) { j++; }
					if (j < tokens.length && tokens[j].type === TokenType.Identifier) {
						return tokens[j].text;
					}
				}
			}
		}
		return "Unknown";
	}

	private extractVariables(tokens: Token[], keyword: string): string[] {
		const vars: string[] = [];
		let startIdx = 0;

		// Find start after keyword
		for (let i = 0; i < tokens.length; i++) {
			if (tokens[i].text.toUpperCase().replace(/^:+/, '') === keyword) {
				startIdx = i + 1;
				break;
			}
		}

		// Collect identifiers separated by commas
		for (let i = startIdx; i < tokens.length; i++) {
			const t = tokens[i];
			if (t.type === TokenType.Identifier) {
				vars.push(t.text);
			}
			// Stop at semicolon?
			if (t.text === ';') { break; }
		}

		return vars;
	}

	private getFirstSignificantToken(node: Node): Token | undefined {
		for (const t of node.tokens) {
			if (t.type !== TokenType.Whitespace && t.type !== TokenType.Comment) { return t; }
		}
		return undefined;
	}
}
