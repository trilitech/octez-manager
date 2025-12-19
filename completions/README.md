# Shell Completions for octez-manager

This directory contains shell completion scripts for `octez-manager`.

## Generating completions

To refresh the completion scripts from the CLI help output:

```bash
dune exec tools/gen_completion.exe -- --binary ./octez-manager
```

This updates:
- `completions/_octez-manager`
- `completions/octez-manager`

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

### Installation

#### Option 1: Automatic installation via opam/dune

If you install `octez-manager` via opam or dune, the completion file is automatically installed to your opam share directory. You just need to enable bash-completion and source the script:

```bash
# Add to your ~/.bashrc:
if [ -f "$(opam var share)/bash-completion/completions/octez-manager" ]; then
  source "$(opam var share)/bash-completion/completions/octez-manager"
fi
```

Then restart your shell or run:
```bash
source ~/.bashrc
```

#### Option 2: System-wide installation (requires sudo)

```bash
sudo cp completions/octez-manager /usr/share/bash-completion/completions/
```

Then restart your shell or run:
```bash
source /etc/bash_completion
```

#### Option 3: User-specific installation

1. Create a directory for your completions if it doesn't exist:
   ```bash
   mkdir -p ~/.bash_completion.d
   ```

2. Copy the completion file:
   ```bash
   cp completions/octez-manager ~/.bash_completion.d/
   ```

3. Add the following to your `~/.bashrc`:
   ```bash
   if [ -f ~/.bash_completion.d/octez-manager ]; then
     source ~/.bash_completion.d/octez-manager
   fi
   ```

4. Restart your shell or run:
   ```bash
   source ~/.bashrc
   ```

#### Option 4: Direct sourcing

Add this line to your `~/.bashrc`:
```bash
source /path/to/octez-manager/completions/octez-manager
```

Then restart your shell or run:
```bash
source ~/.bashrc
```

### Usage

Once installed, you can use tab completion with `octez-manager`:

```bash
octez-manager <TAB>              # Shows all available commands
octez-manager install-node --<TAB>  # Shows all options for install-node
octez-manager instance <TAB>     # Prompts for instance name
octez-manager instance mynode <TAB>  # Shows available actions for the instance
```

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
