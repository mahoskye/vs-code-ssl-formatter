# GitHub Actions Workflows

This directory contains automated CI/CD workflows for the SSL VS Code Extension.

## 🔄 Workflows

### 1. CI (Continuous Integration) - `ci.yml`

**Triggers:**
- Push to `master` or `develop` branches
- Pull requests to `master` or `develop` branches

**What it does:**
- Tests on multiple platforms: Ubuntu, Windows, macOS
- Tests on Node.js versions: 18, 20
- Runs linting, compilation, and unit tests
- Checks for console.log in production code
- Checks for TODO/FIXME comments
- Packages the extension and uploads as artifact

**Status Badge:**
```markdown
![CI](https://github.com/mahoskye/vs-code-ssl-formatter/workflows/CI/badge.svg)
```

### 2. Publish - `publish.yml`

**Triggers:**
- Automatic: When a GitHub release is published
- Manual: Via workflow dispatch in GitHub Actions UI

**What it does:**
- Runs full test suite before publishing
- Publishes to VS Code Marketplace (on release)
- Creates VSIX package (on manual dispatch)

**Setup Required:**
1. Create a Personal Access Token (PAT) for VS Code Marketplace:
   - Go to https://dev.azure.com/
   - Create a PAT with Marketplace > Manage permissions
2. Add the PAT to GitHub Secrets:
   - Go to repo Settings > Secrets and variables > Actions
   - Create new secret: `VSCE_PAT` with your token value

**To Publish:**
1. Update version in `package.json`
2. Create a new GitHub Release with a tag (e.g., `v0.4.1`)
3. Workflow runs automatically and publishes to marketplace

### 3. CodeQL Security Scan - `codeql.yml`

**Triggers:**
- Push to `master` branch
- Pull requests to `master` branch
- Weekly on Mondays at 00:00 UTC

**What it does:**
- Analyzes code for security vulnerabilities
- Detects common coding errors
- Reports found issues in Security tab

## 📊 Viewing Results

### CI Status
- View test results in the "Actions" tab of your GitHub repository
- Each commit/PR will show a checkmark (✅) or X (❌)

### Artifacts
- After CI runs, download the packaged `.vsix` file from the workflow run
- Useful for testing before official release

### Security Alerts
- View CodeQL results in the "Security" > "Code scanning" tab
- GitHub will create alerts for any issues found

## 🛠️ Local Testing

Before pushing, ensure all checks pass locally:

```bash
# Run the same checks as CI
npm run lint
npm run compile
npm run test:unit

# Check for console.log
grep -r "console\.log" src/ --include="*.ts" --exclude="**/logger.ts"

# Check for TODOs
grep -r "TODO\|FIXME" src/ --include="*.ts"
```

## 🚀 Manual Package Creation

To create a VSIX package locally:

```bash
# Install vsce globally
npm install -g @vscode/vsce

# Package the extension
vsce package

# This creates a .vsix file you can install manually in VS Code
```

## 📝 Notes

- CI runs on all platforms to catch platform-specific issues
- Matrix testing ensures compatibility across Node.js versions
- The publish workflow requires `VSCE_PAT` secret to be configured
- All workflows use `npm ci` for faster, reproducible installs
- Artifacts are retained for 30 days
