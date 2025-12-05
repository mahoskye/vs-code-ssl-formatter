# Workspace Navigation Fixture

These files let you manually test **Find All References** with the namespace-aware procedure index (Issue #16). They mirror the calling rules documented in `ssl_agent_instructions.md`:

- Use `DoProc("ProcedureName", { ... })` when calling procedures defined in the same file.
- Use `ExecFunction("Namespace.Script.Procedure", { ... })` when invoking procedures defined in other files.

## Layout

```
workspace/
  MainScript.ssl                # entry point (DoProc + ExecFunction calls)
  reporting/
    GenerateReport.ssl          # contains GenerateReport + GenerateSummary
  services/
    OrderPipeline.ssl           # contains ProcessOrder + ValidateOrder
```

## How to use the fixture

1. In VS Code, open `tests/fixtures/navigation/workspace` as your workspace folder. This keeps all relative paths short and predictable.
2. Add the following setting so the namespace resolver knows where to look:

```json
"ssl.documentNamespaces": {
  "Reporting": "reporting",
  "Services": "services"
}
```

3. Open the files and try **Find All References** (`Shift+F12`) on:
   - `LocalHelper` inside `MainScript.ssl` (tests same-file DoProc handling).
   - `GenerateReport` or `GenerateSummary` string literals inside `MainScript.ssl` or `OrderPipeline.ssl` (tests namespace resolution to `reporting/GenerateReport.ssl`).
   - `ProcessOrder` literal inside `MainScript.ssl` (tests nested namespace segments `Services.OrderPipeline.ProcessOrder`).

You should see both the local string literal call sites *and* the resolved definition in the target file thanks to the shared procedure index. This mirrors how users are expected to structure scripts per `ssl_agent_instructions.md`.
