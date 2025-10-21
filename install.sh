#!/usr/bin/env bash

# Funmacs Interactive Installer
# This script clones the funmacs Emacs configuration from GitHub
# and installs it to ~/.config/emacs

set -e # Exit on error

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Repository and target directory
REPO_URL="https://github.com/mujaxso/funmacs.git"
TARGET_DIR="$HOME/.config/emacs"
OLD_EMACS_DIR="$HOME/.emacs.d"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="$HOME/.config/emacs.backup.$TIMESTAMP"
OLD_BACKUP_DIR="$HOME/.emacs.d.backup.$TIMESTAMP"

# Print colored message
print_message() {
  local color=$1
  local message=$2
  echo -e "${color}${message}${NC}"
}

# Print header
print_header() {
  echo ""
  print_message "$BLUE" "╔═══════════════════════════════════════════╗"
  print_message "$BLUE" "║     Funmacs Configuration Installer       ║"
  print_message "$BLUE" "╚═══════════════════════════════════════════╝"
  echo ""
}

# Check if git is installed
check_git() {
  if ! command -v git &>/dev/null; then
    print_message "$RED" "Error: git is not installed. Please install git first."
    exit 1
  fi
}

# Prompt for user confirmation
prompt_user() {
  local prompt_message=$1
  while true; do
    read -p "$(echo -e ${YELLOW}${prompt_message}${NC}) " response
    case $response in
    [Yy]*) return 0 ;;
    [Nn]*) return 1 ;;
    *) echo "Please answer yes (y) or no (n)." ;;
    esac
  done
}

# Backup old .emacs.d directory
backup_old_emacs_d() {
  if [ -d "$OLD_EMACS_DIR" ] || [ -f "$OLD_EMACS_DIR" ]; then
    print_message "$YELLOW" "Found existing ~/.emacs.d directory"
    print_message "$YELLOW" "This can conflict with ~/.config/emacs"
    echo ""

    if prompt_user "Do you want to backup ~/.emacs.d? (y/n):"; then
      print_message "$BLUE" "Creating backup at $OLD_BACKUP_DIR..."
      mv "$OLD_EMACS_DIR" "$OLD_BACKUP_DIR"
      print_message "$GREEN" "✓ ~/.emacs.d backed up successfully!"
      echo ""
      return 0
    else
      if prompt_user "Do you want to remove ~/.emacs.d? (y/n):"; then
        print_message "$BLUE" "Removing ~/.emacs.d..."
        rm -rf "$OLD_EMACS_DIR"
        print_message "$GREEN" "✓ ~/.emacs.d removed!"
        echo ""
        return 0
      else
        print_message "$YELLOW" "⚠ Warning: ~/.emacs.d will be kept but may conflict with the new configuration"
        echo ""
      fi
    fi
  fi
}

# Backup existing .config/emacs configuration
backup_existing() {
  if [ -d "$TARGET_DIR" ] || [ -f "$TARGET_DIR" ]; then
    print_message "$YELLOW" "Existing Emacs configuration found at $TARGET_DIR"
    echo ""

    if prompt_user "Do you want to backup the existing configuration? (y/n):"; then
      print_message "$BLUE" "Creating backup at $BACKUP_DIR..."
      mv "$TARGET_DIR" "$BACKUP_DIR"
      print_message "$GREEN" "✓ Backup created successfully!"
      echo ""
      return 0
    else
      if prompt_user "Do you want to remove the existing configuration? (y/n):"; then
        print_message "$BLUE" "Removing existing configuration..."
        rm -rf "$TARGET_DIR"
        print_message "$GREEN" "✓ Existing configuration removed!"
        echo ""
        return 0
      else
        print_message "$RED" "Installation cancelled. Cannot proceed without removing or backing up existing configuration."
        exit 1
      fi
    fi
  fi
}

# Clone the repository
clone_repository() {
  print_message "$BLUE" "Cloning funmacs from GitHub..."
  echo ""

  # Create parent directory if it doesn't exist
  mkdir -p "$(dirname "$TARGET_DIR")"

  # Clone with progress
  if git clone "$REPO_URL" "$TARGET_DIR"; then
    print_message "$GREEN" "✓ Repository cloned successfully!"
    echo ""
  else
    print_message "$RED" "✗ Failed to clone repository."
    print_message "$RED" "Please check your internet connection and try again."
    exit 1
  fi
}

# Show installation summary
show_summary() {
  print_message "$GREEN" "╔═══════════════════════════════════════════╗"
  print_message "$GREEN" "║     Installation Completed Successfully   ║"
  print_message "$GREEN" "╚═══════════════════════════════════════════╝"
  echo ""
  print_message "$BLUE" "Installation Details:"
  echo "  • Repository: $REPO_URL"
  echo "  • Location: $TARGET_DIR"

  if [ -d "$BACKUP_DIR" ]; then
    echo "  • Config Backup: $BACKUP_DIR"
  fi

  if [ -d "$OLD_BACKUP_DIR" ]; then
    echo "  • .emacs.d Backup: $OLD_BACKUP_DIR"
  fi

  echo ""
  print_message "$YELLOW" "Next Steps:"
  echo "  1. Launch Emacs to initialize funmacs"
  echo "  2. Emacs will automatically install packages on first launch"
  echo "  3. Be patient during the initial setup"
  echo ""

  if [ -d "$OLD_EMACS_DIR" ]; then
    print_message "$YELLOW" "⚠ Note: ~/.emacs.d still exists and may take priority over ~/.config/emacs"
    print_message "$YELLOW" "   Consider removing it if you experience issues"
    echo ""
  fi

  if prompt_user "Would you like to start Emacs now? (y/n):"; then
    print_message "$BLUE" "Starting Emacs..."
    emacs &
  else
    print_message "$GREEN" "You can start Emacs manually when ready."
  fi
}

# Main installation process
main() {
  print_header

  # Welcome message
  print_message "$BLUE" "This script will install funmacs Emacs configuration"
  print_message "$BLUE" "from GitHub to ~/.config/emacs"
  echo ""
  print_message "$YELLOW" "Note: Emacs checks for configuration in this order:"
  echo "  1. ~/.emacs.d"
  echo "  2. ~/.config/emacs"
  echo ""
  print_message "$YELLOW" "This installer will help you backup/remove both directories"
  print_message "$YELLOW" "to avoid conflicts."
  echo ""

  if ! prompt_user "Do you want to continue? (y/n):"; then
    print_message "$YELLOW" "Installation cancelled by user."
    exit 0
  fi

  echo ""

  # Run installation steps
  check_git
  backup_old_emacs_d
  backup_existing
  clone_repository
  show_summary
}

# Run main function
main
