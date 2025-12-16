# Shell Completions for octez-manager

This directory contains shell completion scripts for `octez-manager`.

## Zsh Completion

### Installation

There are several ways to install the zsh completion:

#### Option 1: Automatic installation via opam/dune

If you install `octez-manager` via opam or dune, the completion file is automatically installed to your opam share directory. You just need to add the opam zsh completions to your fpath:

```bash
# Add to your ~/.zshrc (before compinit is called):
fpath=($(opam var share)/zsh/site-functions $fpath)
```

Then restart your shell or run:
```bash
source ~/.zshrc
```

#### Option 2: System-wide installation (requires sudo)

```bash
sudo cp completions/_octez-manager /usr/share/zsh/site-functions/
```

Then restart your shell or run:
```bash
compinit
```

#### Option 3: User-specific installation

1. Create a directory for your completions if it doesn't exist:
   ```bash
   mkdir -p ~/.zsh/completions
   ```

2. Copy the completion file:
   ```bash
   cp completions/_octez-manager ~/.zsh/completions/
   ```

3. Add the following to your `~/.zshrc` (before `compinit` is called):
   ```bash
   fpath=(~/.zsh/completions $fpath)
   ```

4. Restart your shell or run:
   ```bash
   source ~/.zshrc
   ```

#### Option 4: Direct sourcing

Add this line to your `~/.zshrc`:
```bash
source /path/to/octez-manager/completions/_octez-manager
```

Then restart your shell or run:
```bash
source ~/.zshrc
```

### Usage

Once installed, you can use tab completion with `octez-manager`:

```bash
octez-manager <TAB>              # Shows all available commands
octez-manager install-node --<TAB>  # Shows all options for install-node
octez-manager instance <TAB>     # Prompts for instance name
octez-manager instance mynode <TAB>  # Shows available actions for the instance
```

The completion script provides:
- Command completion for all octez-manager commands
- Option completion for all command-line flags
- Intelligent suggestions for values like history modes, snapshot kinds, and liquidity baking votes
- File and directory path completion where appropriate
- User completion for `--service-user` options

## Bash Completion

Bash completion support may be added in the future. Contributions are welcome!

## Troubleshooting

### Completion not working

1. Make sure `compinit` is called in your `.zshrc` after adding the completion directory to `fpath`.

2. Clear the completion cache:
   ```bash
   rm -f ~/.zcompdump
   compinit
   ```

3. Check that the completion file is in your `fpath`:
   ```bash
   echo $fpath
   ```

4. Verify zsh can find the completion:
   ```bash
   which _octez-manager
   ```

### Updating completions

If you update the completion script, you may need to clear the cache:
```bash
rm -f ~/.zcompdump
exec zsh
```
