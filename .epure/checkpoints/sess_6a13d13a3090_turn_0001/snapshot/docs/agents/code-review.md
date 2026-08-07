# Code Review Guidelines

Reference guide for reviewing pull requests. See also the root [AGENTS.md](../../AGENTS.md).

## Code Review Guidelines

When reviewing PRs:

### Focus on Issues Only

- **Do:** Point out bugs, architectural problems, performance issues
- **Don't:** Praise what works well - assume good code is expected
- **Don't:** State that tests pass - CI already validates this

### Be Concise

- Use bullet points
- One issue per bullet
- Include line numbers for specific problems
- Provide fix suggestions, not explanations of the problem

### Review Format

```markdown
## Review

### BLOCKER 🔴
- Issue description (line X)
- **Fix:** Concrete solution

### Issues
- Problem 1 (line Y)
- Problem 2 (lines Z-W)

### Questions
- Clarification needed on X
```

### What to Skip

- ❌ "What's great" sections
- ❌ Testing reports (CI handles this)
- ❌ Praise or encouragement
- ❌ Long explanations of why something is wrong
- ❌ Multiple comments - use one comment with bullets

### What to Include

- ✅ Specific line numbers
- ✅ Concrete fix suggestions
- ✅ Links to correct patterns in codebase
- ✅ Severity indicators (BLOCKER, issue, question)

