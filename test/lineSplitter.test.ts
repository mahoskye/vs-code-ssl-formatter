import * as assert from "assert";
import * as vscode from "vscode";
import { LineSplitter } from "../src/formatters/lineSplitter";
import { FormatterPipeline, TypedBlock } from "../src/formatters/formattingPipeline";

suite("LineSplitter Test Suite", () => {
    vscode.window.showInformationMessage("Start all tests.");

    const createTestPipeline = (maxLineLength: number = 80, indentSize: number = 6) => {
        const pipeline = new FormatterPipeline();
        const lineSplitter = new LineSplitter(maxLineLength, indentSize);
        pipeline.addFormatter(lineSplitter);
        return { pipeline, lineSplitter };
    };

    const formatText = async (text: string, maxLineLength: number = 80, indentSize: number = 6) => {
        const { pipeline, lineSplitter } = createTestPipeline(maxLineLength, indentSize);
        const blocks = pipeline["blockProcessor"].processText(text);
        await lineSplitter.format(blocks);
        return pipeline["blockProcessor"].blocksToText(blocks);
    };

    test("String concatenation as part of a function call", async () => {
        const input = `SQLExecute("SELECT * FROM EQUIPMENT WHERE EQUIPID='"+sEquipId+"' AND TESTCODE="+LimsString(nTestCode));`;
        const expected = `SQLExecute("SELECT * FROM EQUIPMENT WHERE EQUIPID='"+sEquipId+"' AND TESTCODE="
+LimsString(nTestCode));
`;

        const result = await formatText(input);
        assert.strictEqual(result, expected);
    });

    //     test("Long keyword lists", async () => {
    //         const input = `:PARAMETERS SESSIONID,versionControl,level,debug,logPath,logFile,bLog,crlf,tab,runType,exclude,indicatorCol,extractSql,updateSql;`;
    //         const expected = `:PARAMETERS SESSIONID,versionControl,level,debug,logPath,logFile,
    // bLog,crlf,tab,runType,exclude,indicatorCol,
    // extractSql,updateSql;
    // `;

    //         const result = await formatText(input);
    //         assert.strictEqual(result, expected);
    //     });

    //     test("Long object lists (with extra whitespace)", async () => {
    //         const input = `exclude := {"CPROW","CPCAL","MINDC","MSMRL","UNCRT","CPPDT","CPADT","QCNC","QCEXP","QCMISS" };  `;
    //         const expected = `exclude := {
    // "CPROW","CPCAL","MINDC","MSMRL","UNCRT","CPPDT",
    // "CPADT","QCNC","QCEXP","QCMISS"
    //  };
    // `;

    //         const result = await formatText(input);
    //         assert.strictEqual(result, expected);
    //     });

    test("Indented long function call", async () => {
        const input = `		uDummy := ExecFunction("USGS_ExtractionHelpers.Updates.UpdateInvID_FromLotno", {SESSIONID});`;
        const expected = `		uDummy := ExecFunction("USGS_ExtractionHelpers.Updates.UpdateInvID_FromLotno",
 {SESSIONID});
`;

        const result = await formatText(input);
        assert.strictEqual(result, expected);
    });

    //     test("Indented long function call with comment at the end", async () => {
    //         const input = `		uDummy := ExecFunction("USGS_ExtractionHelpers.Updates.UpdateInvID_FromLotno", {SESSIONID}); /*updates Inventory ID stuff from LOTNO;`;
    //         const expected = `/*updates Inventory ID stuff from LOTNO;
    // 		uDummy := ExecFunction("USGS_ExtractionHelpers.Updates.UpdateInvID_FromLotno",
    //  {SESSIONID});
    // `;

    //         const result = await formatText(input);
    //         assert.strictEqual(result, expected);
    //     });

    test("Comment handling", async () => {
        const input = `/*veryTemporary - the last three are being checked in special QC scripts for IMP and EXP - not sure if we need to include these in a cross runtype way;`;
        const expected = input + "\n";

        const result = await formatText(input);
        assert.strictEqual(result, expected);
    });
});
