# Recruiter Changelog

Durable release history for the recruiter agent installed under `.claude/`.
Source roster: [mathiasbourgoin/agent-roster](https://github.com/mathiasbourgoin/agent-roster).

## v2.0.0 — Team-First Philosophy Reframe

The project's purpose shifted from "a registry of reusable agent components" to
"a harness for fast and correct development with productive teams."

- **Agents cannot spawn agents.** Hard platform constraint. The human (or
  orchestrating Claude) is always the relay. Two execution modes: Mode A
  (parallel, all agents at once) and Mode B (human-mediated sequential, default).
- **Human validation is mandatory.** Plans, briefs, and proposals require a
  structured quiz — not a one-word "yes". Defined in
  `.claude/rules/human-validation.md`.
- **Research → brief → planner pipeline.** Tech-lead does a research phase,
  compresses findings into `briefs/<task>-research-brief.md`, hands off to a
  fresh planner. The planner produces sub-briefs per execution agent.
- **Spawn requests are concrete.** `SPAWN REQUEST` blocks embed the full prompt
  inline — no file paths passed alone.
- **Planner is a new agent.** Task decomposition in a fresh context.
- **Lead is mandatory.** No team without a lead.
- **Three-layer install.** Tunables + pipeline patch + lead/adjacency updates.
