# Async_Result Library Tools

This directory contains utility scripts and tools for maintaining the Async_Result library.

## Available Tools

### cleanup.sh
A comprehensive cleanup script that removes temporary files, build artifacts, and other disposable files.

**Usage:**
```bash
./tools/cleanup.sh
```

Or via Makefile:
```bash
make clean-temp    # Clean temporary files only
make pristine      # Restore to pristine state (removes all generated files)
```

**What it removes:**
- Backup files (*.backup, *.bak, *.orig, *~)
- System metadata (.DS_Store, Thumbs.db)
- Editor temporary files (*.swp, *.swo)
- Alire temporary files
- Build artifacts (obj/, lib/, build/)
- Test executables
- Coverage reports
- Log files

**What it preserves:**
- Source code (*.ads, *.adb)
- Configuration files (*.toml, *.gpr, Makefile)
- Documentation (*.md)
- Essential project structure

## Adding New Tools

When adding new tools:
1. Create the script in this directory
2. Make it executable: `chmod +x tools/your-script.sh`
3. Add a Makefile target if appropriate
4. Document it in this README

## Tool Guidelines

- Use bash for portability
- Include proper error handling
- Provide colored output for better UX
- Always work from project root
- Document what the tool does in comments
- Test on multiple platforms when possible