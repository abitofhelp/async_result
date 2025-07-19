#!/bin/bash
# =============================================================================
# Ada Async_Result Library - Cleanup Tool
# Copyright (c) 2025 A Bit of Help, Inc.
# SPDX-License-Identifier: MIT
#
# Removes temporary files, build artifacts, and other disposable files
# =============================================================================

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

# Change to project root
cd "$PROJECT_ROOT"

echo -e "${BLUE}🧹 Ada Async_Result Library - Cleanup Tool${NC}"
echo -e "${CYAN}Cleaning temporary and build files...${NC}"
echo ""

# Function to safely remove files/directories
safe_remove() {
    local target="$1"
    local description="$2"
    
    if [ -e "$target" ]; then
        rm -rf "$target"
        echo -e "${GREEN}✓${NC} Removed $description"
    fi
}

# Backup files
echo -e "${YELLOW}Removing backup files...${NC}"
find . -name "*.backup" -o -name "*.bak" -o -name "*.orig" -o -name "*~" | while read -r file; do
    safe_remove "$file" "backup: $(basename "$file")"
done

# macOS metadata
echo -e "\n${YELLOW}Removing system metadata...${NC}"
find . -name ".DS_Store" -delete 2>/dev/null && echo -e "${GREEN}✓${NC} Removed .DS_Store files"
find . -name "Thumbs.db" -delete 2>/dev/null && echo -e "${GREEN}✓${NC} Removed Thumbs.db files"

# Editor temporary files
echo -e "\n${YELLOW}Removing editor temporary files...${NC}"
find . -name "*.swp" -o -name "*.swo" -o -name "*~" | while read -r file; do
    safe_remove "$file" "editor temp: $(basename "$file")"
done

# Alire temporary files
echo -e "\n${YELLOW}Removing Alire temporary files...${NC}"
safe_remove "alire/tmp/*.tmp" "Alire temp files"
safe_remove "alire/build_hash_inputs" "Alire build hash"

# Build artifacts
echo -e "\n${YELLOW}Removing build artifacts...${NC}"
safe_remove "obj/" "object files directory"
safe_remove "lib/" "library directory"
safe_remove "build/" "build directory"

# Test executables
echo -e "\n${YELLOW}Removing test executables...${NC}"
find tests -type f -perm +111 ! -name "*.sh" ! -name "*.gpr" 2>/dev/null | while read -r exe; do
    safe_remove "$exe" "test executable: $(basename "$exe")"
done

# Debug and test files in root directory
echo -e "\n${YELLOW}Removing debug and test files from root...${NC}"
for pattern in "debug_*" "test_hang*" "minimal_*"; do
    for file in $pattern; do
        if [ -f "$file" ]; then
            safe_remove "$file" "debug/test file: $file"
        fi
    done
done

# Remove associated project files for debug tests
for gpr in debug_*.gpr test_*.gpr minimal_*.gpr hang_*.gpr; do
    if [ -f "$gpr" ]; then
        safe_remove "$gpr" "debug project file: $gpr"
    fi
done

# Coverage reports
echo -e "\n${YELLOW}Removing coverage reports...${NC}"
safe_remove "coverage/" "coverage directory"
safe_remove "*.gcda" "coverage data files"
safe_remove "*.gcno" "coverage note files"

# Log files
echo -e "\n${YELLOW}Removing log files...${NC}"
find . -name "*.log" -o -name "*.out" | while read -r file; do
    safe_remove "$file" "log: $(basename "$file")"
done

# Summary
echo -e "\n${BLUE}📊 Cleanup Summary${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

# Count remaining files
ada_files=$(find src -name "*.ad[sb]" 2>/dev/null | wc -l | tr -d ' ')
test_files=$(find tests -name "*.ad[sb]" 2>/dev/null | wc -l | tr -d ' ')
total_size=$(du -sh . 2>/dev/null | cut -f1)

echo -e "Ada source files:  ${GREEN}$ada_files${NC}"
echo -e "Test files:        ${GREEN}$test_files${NC}"
echo -e "Project size:      ${GREEN}$total_size${NC}"

echo -e "\n${GREEN}✅ Cleanup complete!${NC}"
echo -e "${YELLOW}💡 Run 'alr build' to rebuild the library${NC}"
echo -e "${YELLOW}💡 Run 'make test' to rebuild and run tests${NC}"