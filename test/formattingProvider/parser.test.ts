import * as assert from "assert";
import { describe, it } from "mocha";
import { SSLParser, ASTNode, ASTNodeType } from "../../src/formatters/parser";
import { SSLTokenizer, Token } from "../../src/formatters/tokenizer";

describe("SSLParser", () => {
    const tokenize = (text: string): Token[] => {
        const tokenizer = new SSLTokenizer(text);
        return tokenizer.tokenize();
    };

    const parse = (text: string): ASTNode => {
        const tokens = tokenize(text);
        const parser = new SSLParser(tokens);
        return parser.parse();
    };

    it("should parse an empty program", () => {
        const ast = parse("");
        assert.deepStrictEqual(ast.type, ASTNodeType.program);
        assert.deepStrictEqual(ast.children.length, 0);
    });

    it("should parse a simple procedure", () => {
        const code = `
            :PROCEDURE MyProc
            :ENDPROC
        `;
        const ast = parse(code);
        assert.strictEqual(ast.children.length, 1);
        const procNode = ast.children[0];
        assert.strictEqual(procNode.type, ASTNodeType.procedure);
        assert.ok(
            procNode.children.some(
                (child) => child.type === ASTNodeType.identifier && child.value === "MyProc"
            )
        );
    });

    it("should parse a procedure with parameters", () => {
        const code = `
            :PROCEDURE MyProcWithParams :PARAMETERS p1, p2
            :ENDPROC
        `;
        const ast = parse(code);
        const procNode = ast.children[0];
        assert.strictEqual(procNode.type, ASTNodeType.procedure);
        const paramsNode = procNode.children.find((c) => c.type === ASTNodeType.parameter);
        assert.ok(paramsNode, "Parameters node not found");
        if (paramsNode) {
            assert.strictEqual(paramsNode.children.length, 2);
            assert.strictEqual(paramsNode.children[0].value, "p1");
            assert.strictEqual(paramsNode.children[1].value, "p2");
        }
    });

    it("should parse a declaration statement", () => {
        const code = ":DECLARE myVar, anotherVar";
        const ast = parse(code);
        const declareNode = ast.children[0];
        assert.strictEqual(declareNode.type, ASTNodeType.declaration);
        assert.strictEqual(declareNode.children.length, 2);
        assert.strictEqual(declareNode.children[0].value, "myVar");
        assert.strictEqual(declareNode.children[1].value, "anotherVar");
    });

    it("should parse an IF statement", () => {
        const code = `
            :IF x > 10
                y := 1
            :ELSE
                y := 2
            :ENDIF
        `;
        const ast = parse(code);
        const ifNode = ast.children[0];
        assert.strictEqual(ifNode.type, ASTNodeType.ifStatement);
        assert.strictEqual(ifNode.children.length, 3); // Condition, Then block, Else block
        assert.strictEqual(ifNode.children[0].type, ASTNodeType.binaryExpression); // Condition
        assert.strictEqual(ifNode.children[1].type, ASTNodeType.blockStatement); // Then
        assert.strictEqual(ifNode.children[2].type, ASTNodeType.blockStatement); // Else
    });

    it("should parse a WHILE statement", () => {
        const code = `
            :WHILE i < 5
                i := i + 1
            :ENDWHILE
        `;
        const ast = parse(code);
        const whileNode = ast.children[0];
        assert.strictEqual(whileNode.type, ASTNodeType.whileStatement);
        assert.strictEqual(whileNode.children.length, 2); // Condition, Body block
    });

    it("should parse a FOR statement", () => {
        const code = `
            :FOR j := 1 :TO 10
                x := j
            :NEXT
        `;
        const ast = parse(code);
        assert.ok(ast.children.some((c) => c.type === ASTNodeType.forStatement));
        const forNode = ast.children.find((c) => c.type === ASTNodeType.forStatement);
        assert.ok(forNode);
    });

    it("should parse a BEGINCASE statement", () => {
        const code = `
            :BEGINCASE
            :CASE a == 1
                res := "one"
            :OTHERWISE
                res := "other"
            :ENDCASE
        `;
        const ast = parse(code);
        const caseNode = ast.children[0];
        assert.strictEqual(caseNode.type, ASTNodeType.beginCaseStatement);
        assert.ok(caseNode.children.length >= 2);
        assert.strictEqual(caseNode.children[0].type, ASTNodeType.caseStatement);
        assert.strictEqual(caseNode.children[1].type, ASTNodeType.caseStatement);
    });

    it("should parse a TRY-CATCH-FINALLY statement", () => {
        const code = `
            :TRY
                DoSomething()
            :CATCH e
                LogError(e)
            :FINALLY
                CleanUp()
            :ENDTRY
        `;
        const ast = parse(code);
        const tryNode = ast.children[0];
        assert.strictEqual(tryNode.type, ASTNodeType.tryStatement);
        assert.strictEqual(tryNode.children.length, 3);
    });

    it("should parse an assignment expression", () => {
        const code = "myVar := 10 + 5";
        const ast = parse(code);
        const stmtNode = ast.children[0];
        assert.strictEqual(stmtNode.type, ASTNodeType.statement);
        const assignmentNode = stmtNode.children[0];
        assert.strictEqual(assignmentNode.type, ASTNodeType.assignmentExpression);
        assert.strictEqual(assignmentNode.value, ":=");
        assert.strictEqual(assignmentNode.children[0].type, ASTNodeType.identifier);
        assert.strictEqual(assignmentNode.children[0].value, "myVar");
        assert.strictEqual(assignmentNode.children[1].type, ASTNodeType.binaryExpression);
    });

    it("should parse a binary expression", () => {
        const code = "a + b * c";
        const ast = parse(code);
        const stmtNode = ast.children[0];
        const binaryNode = stmtNode.children[0];
        assert.strictEqual(binaryNode.type, ASTNodeType.binaryExpression);
        assert.strictEqual(binaryNode.value, "+");
        assert.strictEqual(binaryNode.children[0].type, ASTNodeType.identifier);
        assert.strictEqual(binaryNode.children[1].type, ASTNodeType.binaryExpression);
        const nestedBinaryNode = binaryNode.children[1];
        assert.strictEqual(nestedBinaryNode.value, "*");
    });

    it("should parse a unary expression", () => {
        const code = ".NOT. flag";
        const ast = parse(code);
        const stmtNode = ast.children[0];
        const unaryNode = stmtNode.children[0];
        assert.strictEqual(unaryNode.type, ASTNodeType.unaryExpression);
        assert.strictEqual(unaryNode.value, ".NOT.");
        assert.strictEqual(unaryNode.children[0].type, ASTNodeType.identifier);
        assert.strictEqual(unaryNode.children[0].value, "flag");
    });

    it("should parse literals", () => {
        const code = `
            s := "hello"
            n := 123
            b := .T.
            nilVal := NIL
        `;
        const ast = parse(code);
        assert.strictEqual(ast.children.length, 4);

        const sAssign = ast.children[0].children[0];
        assert.strictEqual(sAssign.children[1].type, ASTNodeType.literal);
        assert.strictEqual(sAssign.children[1].value, '"hello"');

        const nAssign = ast.children[1].children[0];
        assert.strictEqual(nAssign.children[1].type, ASTNodeType.literal);
        assert.strictEqual(nAssign.children[1].value, "123");

        const bAssign = ast.children[2].children[0];
        assert.strictEqual(bAssign.children[1].type, ASTNodeType.literal);
        assert.strictEqual(bAssign.children[1].value, ".T.");

        const nilAssign = ast.children[3].children[0];
        assert.strictEqual(nilAssign.children[1].type, ASTNodeType.literal);
        assert.strictEqual(nilAssign.children[1].value, "NIL");
    });

    it("should parse comments and ignore them in the main AST structure", () => {
        const code = `
            /* This is a block comment */
            :PROCEDURE TestProc
                var1 := 10 /* End of line comment */
            :ENDPROC
            /* Another comment */
        `;
        const ast = parse(code);
        assert.strictEqual(ast.children.length, 1);
        assert.strictEqual(ast.children[0].type, ASTNodeType.procedure);

        const procNode = ast.children[0];
        const assignmentStatement = procNode.children.find(
            (child: ASTNode) =>
                child.type === ASTNodeType.statement &&
                child.children[0]?.type === ASTNodeType.assignmentExpression
        );
        assert.ok(assignmentStatement, "Assignment statement not found in procedure");
    });

    it("should parse nested structures correctly", () => {
        const code = `
            :PROCEDURE NestedExample
                :IF condition1
                    :WHILE condition2
                        :IF condition3
                            x := 1
                        :ENDIF
                    :ENDWHILE
                :ENDIF
            :ENDPROC
        `;
        const ast = parse(code);
        const procNode = ast.children[0];
        const ifNode = procNode.children.find((c) => c.type === ASTNodeType.ifStatement);
        assert.ok(ifNode);
        const ifBlock = ifNode.children.find((c) => c.type === ASTNodeType.blockStatement);
        assert.ok(ifBlock);
        const whileNode = ifBlock.children.find((c) => c.type === ASTNodeType.whileStatement);
        assert.ok(whileNode);
        const whileBlock = whileNode.children.find((c) => c.type === ASTNodeType.blockStatement);
        assert.ok(whileBlock);
        const innerIfNode = whileBlock.children.find((c) => c.type === ASTNodeType.ifStatement);
        assert.ok(innerIfNode);
    });

    it("should handle malformed input gracefully (skips problematic tokens)", () => {
        const code = `
            :PROCEDURE Malformed
                var1 := 10
                :::INVALID_TOKEN::: <<>> 
                var2 := 20
            :ENDPROC
        `;
        const ast = parse(code);
        const procNode = ast.children[0];
        assert.strictEqual(procNode.type, ASTNodeType.procedure);
        const assignmentStatements = procNode.children.filter(
            (child: ASTNode) =>
                child.type === ASTNodeType.statement &&
                child.children[0]?.type === ASTNodeType.assignmentExpression
        );
        assert.strictEqual(
            assignmentStatements.length,
            2,
            "Should find two valid assignment statements"
        );
        assert.strictEqual(assignmentStatements[0].children[0].children[0].value, "var1");
        assert.strictEqual(assignmentStatements[1].children[0].children[0].value, "var2");
    });

    it("should parse REGION statements", () => {
        const code = `
            :REGION MyRegion
                :DECLARE regVar
            :ENDREGION
        `;
        const ast = parse(code);
        const regionNode = ast.children[0];
        assert.strictEqual(regionNode.type, ASTNodeType.regionStatement);
        assert.ok(regionNode.children.some((c) => c.type === ASTNodeType.declaration));
    });

    it("should parse CLASS statements (basic)", () => {
        const code = `
            :CLASS MyClass
                :DECLARE classVar
        `;
        const ast = parse(code);
        const classNode = ast.children.find((c) => c.type === ASTNodeType.classStatement);
        assert.ok(classNode, "Class node not found");
        if (classNode) {
            assert.strictEqual(classNode.children.length, 1);
            assert.strictEqual(classNode.children[0].value, "MyClass");
        }
        const declareNode = ast.children.find((c) => c.type === ASTNodeType.declaration);
        assert.ok(declareNode, "Declaration node not found as sibling");
    });

    it("should parse an empty input string", () => {
        const ast = parse("");
        assert.strictEqual(ast.type, ASTNodeType.program);
        assert.strictEqual(ast.children.length, 0);
    });

    it("should parse input with only comments", () => {
        const code = `
            /* Comment 1 */
            /* Comment 2
               multiline */
        `;
        const ast = parse(code);
        assert.strictEqual(ast.type, ASTNodeType.program);
        assert.strictEqual(ast.children.length, 0);
    });

    it("should correctly report line numbers for statements (approximate)", () => {
        const code = `
            :PROCEDURE LineTest
                varA := 1  

            :IF .T.      
                varB := 2
            :ENDIF
            :ENDPROC
        `;
        const tokens = tokenize(code);
        const parser = new SSLParser(tokens);
        const ast = parser.parse();

        const procNode = ast.children[0];
        assert.ok(
            procNode.line === tokens.find((t) => t.value === "PROCEDURE")?.position.line,
            "Procedure line number mismatch"
        );

        const firstAssignmentStmt = procNode.children.find(
            (c: ASTNode) =>
                c.type === ASTNodeType.statement && c.children[0]?.children[0]?.value === "varA"
        );
        assert.ok(firstAssignmentStmt, "varA assignment not found");
        const varAToken = tokens.find((t) => t.value === "varA");
        assert.ok(varAToken, "Token varA not found");
        assert.ok(
            firstAssignmentStmt.line === varAToken.position.line,
            `varA line mismatch. Expected around ${varAToken.position.line}, got ${firstAssignmentStmt.line}`
        );

        const ifNode = procNode.children.find((c: ASTNode) => c.type === ASTNodeType.ifStatement);
        assert.ok(ifNode, "IF node not found");
        const ifToken = tokens.find((t) => t.value === "IF");
        assert.ok(ifToken, "Token IF not found");
        assert.ok(
            ifNode.line === ifToken.position.line,
            `IF line mismatch. Expected ${ifToken.position.line}, got ${ifNode.line}`
        );
    });

    it("should parse a complex, deeply nested code structure", () => {
        const code = `
            :PROCEDURE ComplexLogic :PARAMETERS dataObject
                :DECLARE isValid, i, item
                isValid := .F.
                :IF dataObject != NIL .AND. dataObject:HasKey("items")
                    :FOR i := 1 :TO ASize(dataObject:items)
                        item := dataObject:items[i]
                        :IF item:type == "critical" .AND. item:value > 100
                            :TRY
                                ProcessCriticalItem(item)
                                isValid := .T.
                            :CATCH ex
                                LogError("Failed on critical item: " + ex:Message)
                                :RETURN .F. 
                            :ENDTRY
                        :ENDIF
                    :NEXT
                :ENDIF
                :RETURN isValid
            :ENDPROC
        `;
        const ast = parse(code);
        assert.strictEqual(ast.children.length, 1);
        const procNode = ast.children[0];
        assert.strictEqual(procNode.type, ASTNodeType.procedure);
        assert.ok(
            procNode.children.length > 3,
            "Procedure should have multiple children (name, params, declarations, statements)"
        );

        const ifOuterNode = procNode.children.find(
            (c: ASTNode) => c.type === ASTNodeType.ifStatement
        );
        assert.ok(ifOuterNode, "Outer IF not found");

        const forNode = ifOuterNode.children
            .find((c: ASTNode) => c.type === ASTNodeType.blockStatement)
            ?.children.find((c: ASTNode) => c.type === ASTNodeType.forStatement);
        assert.ok(forNode, "FOR loop not found");

        const forBlock = forNode.children.find(
            (c: ASTNode) => c.type === ASTNodeType.blockStatement || Array.isArray(c.children)
        );
        assert.ok(forBlock, "FOR block/body not found");

        const ifInnerNode = forBlock.children.find(
            (c: ASTNode) => c.type === ASTNodeType.ifStatement
        );
        assert.ok(ifInnerNode, "Inner IF not found");

        const tryNode = ifInnerNode.children
            .find((c: ASTNode) => c.type === ASTNodeType.blockStatement)
            ?.children.find((c: ASTNode) => c.type === ASTNodeType.tryStatement);
        assert.ok(tryNode, "TRY statement not found");
        assert.strictEqual(tryNode.children.length, 2, "TRY node should have try and catch blocks");
    });

    it("should parse procedure without a name", () => {
        const code = `:PROCEDURE :PARAMETERS p1 \n :DECLARE x \n x := p1 \n :ENDPROC`;
        const ast = parse(code);
        assert.strictEqual(ast.children.length, 1);
        const procNode = ast.children[0];
        assert.strictEqual(procNode.type, ASTNodeType.procedure);
        assert.ok(
            !procNode.children.some(
                (c) => c.type === ASTNodeType.identifier && procNode.children.indexOf(c) === 0
            ),
            "Procedure should not have a name identifier as first child"
        );
        assert.ok(
            procNode.children.some((c) => c.type === ASTNodeType.parameter),
            "Procedure should have parameters"
        );
        assert.ok(
            procNode.children.some((c) => c.type === ASTNodeType.declaration),
            "Procedure should have declarations"
        );
    });

    it("should parse an IF statement without ELSE", () => {
        const code = `:IF x > 10 \n y := 1 \n :ENDIF`;
        const ast = parse(code);
        const ifNode = ast.children[0];
        assert.strictEqual(ifNode.type, ASTNodeType.ifStatement);
        assert.strictEqual(ifNode.children.length, 2);
        assert.strictEqual(ifNode.children[1].type, ASTNodeType.blockStatement);
    });

    it("should parse a TRY statement with only CATCH", () => {
        const code = `:TRY \n DoA() \n :CATCH err \n HandleError(err) \n :ENDTRY`;
        const ast = parse(code);
        const tryNode = ast.children[0];
        assert.strictEqual(tryNode.type, ASTNodeType.tryStatement);
        assert.strictEqual(tryNode.children.length, 2);
    });

    it("should parse a TRY statement with only FINALLY", () => {
        const code = `:TRY \n DoB() \n :FINALLY \n Clean() \n :ENDTRY`;
        const ast = parse(code);
        const tryNode = ast.children[0];
        assert.strictEqual(tryNode.type, ASTNodeType.tryStatement);
        assert.strictEqual(tryNode.children.length, 2);
        assert.strictEqual(tryNode.children[0].type, ASTNodeType.blockStatement);
        assert.strictEqual(tryNode.children[1].type, ASTNodeType.blockStatement);
    });

    it("should parse chained member expressions and function calls (basic check)", () => {
        const code = "myObject:property:callFunction()";
        const ast = parse(code);
        assert.ok(ast.children.length > 0, "AST should have children for the statement");
        const stmtNode = ast.children[0];
        assert.strictEqual(stmtNode.type, ASTNodeType.statement, "Statement node type mismatch");
        assert.ok(stmtNode.children.length > 0, "Statement node should have children");

        const firstExpr = stmtNode.children[0];
        assert.strictEqual(
            firstExpr.type,
            ASTNodeType.identifier,
            "Expected first part to be an identifier"
        );
        assert.strictEqual(firstExpr.value, "myObject", "Expected identifier value to be myObject");
    });

    it("should parse function calls with arguments (basic check)", () => {
        const code = "MyFunction(arg1, 123)";
        const ast = parse(code);
        assert.ok(ast.children.length > 0, "AST should have children for the call statement");
        const stmtNode = ast.children[0];
        assert.strictEqual(stmtNode.type, ASTNodeType.statement, "Statement node type mismatch");
        assert.ok(stmtNode.children.length > 0, "Statement node should have children");

        const funcIdentifier = stmtNode.children[0];
        assert.strictEqual(
            funcIdentifier.type,
            ASTNodeType.identifier,
            "Expected function name to be an identifier"
        );
        assert.strictEqual(
            funcIdentifier.value,
            "MyFunction",
            "Expected identifier value to be MyFunction"
        );
    });
});

function printAST(node: ASTNode, indent = 0) {
    console.log(
        `${" ".repeat(indent)}Type: ${node.type}, Value: ${node.value || ""}, Line: ${node.line}`
    );
    if (node.children) {
        node.children.forEach((child) => printAST(child, indent + 2));
    }
}
