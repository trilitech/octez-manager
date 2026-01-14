# Testing Zsh Completion

This document describes how to test the shell completion scripts for octez-manager.

## Manual Testing

### Prerequisites
- zsh installed on your system
- bash installed on your system
- octez-manager completion script installed (see completions/README.md)

### Basic Testing

1. **Test command completion**:
   ```zsh
   octez-manager <TAB>
   ```
   Should show all available commands (instance, install-node, install-baker, etc.)

2. **Test option completion**:
   ```zsh
   octez-manager install-node --<TAB>
   ```
   Should show all options for install-node command (--instance, --network, --rpc-addr, etc.)

3. **Test subcommand completion**:
   ```zsh
   octez-manager instance mynode <TAB>
   ```
   Should show instance actions (start, stop, restart, remove, purge, show, show-service)

4. **Test value completion**:
   ```zsh
   octez-manager install-node --history-mode <TAB>
   ```
   Should show history mode options (archive, full, rolling)

   ```zsh
   octez-manager install-baker --liquidity-baking-vote <TAB>
   ```
   Should show voting options (on, off, pass)


### Bash Basic Testing

1. **Test command completion**:
   ```bash
   octez-manager <TAB>
   ```
   Should show all available commands (instance, install-node, install-baker, etc.)

2. **Test option completion**:
   ```bash
   octez-manager install-node --<TAB>
   ```
   Should show all options for install-node command (--instance, --network, --rpc-addr, etc.)

3. **Test subcommand completion**:
   ```bash
   octez-manager instance mynode <TAB>
   ```
   Should show instance actions (start, stop, restart, remove, purge, show, show-service)

4. **Test value completion**:
   ```bash
   octez-manager install-node --history-mode <TAB>
   ```
   Should show history mode options (archive, full, rolling)

   ```bash
   octez-manager install-baker --liquidity-baking-vote <TAB>
   ```
   Should show voting options (on, off, pass)

### Syntax Validation

To check for zsh syntax errors without running the completion:

```zsh
zsh -n completions/octez-manager.zsh
```

This will report syntax errors without executing the script.

To check for bash syntax errors without running the completion:

```bash
bash -n completions/octez-manager.bash
```

### Debugging

To see what the completion system is doing:

```zsh
# Enable completion debugging
setopt prompt_subst
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'

# Then try completions
octez-manager <TAB>
```

## Automated Testing

Currently, there are no automated tests for the completion script. Future enhancements could include:

1. **Unit tests** using zsh's test framework
2. **Integration tests** that verify completions match actual CLI options
3. **Regression tests** to ensure completion behavior remains stable

## Known Limitations

1. **Instance name completion**: The completion currently shows a generic message for instance names. A future enhancement could query `octez-manager list` to provide actual instance names.

2. **Network name completion**: Similarly, network names could be populated from `octez-manager list-available-networks`.

3. **Dynamic completion**: Some completions are static lists. They could be made dynamic by invoking the CLI to fetch current values.

## Future Enhancements

- Add instance name completion by querying the service registry
- Add network name completion from teztnets.com
- Add bash completion support
- Add fish completion support
- Generate completions dynamically from CLI help text
- Add automated tests for completion behavior
