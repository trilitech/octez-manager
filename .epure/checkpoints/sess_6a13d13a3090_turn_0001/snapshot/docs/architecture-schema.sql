-- Architecture Index Schema
-- This database provides a queryable index of the codebase for gardening purposes.
-- Location: docs/architecture.db

PRAGMA foreign_keys = ON;

-- Modules (source files)
CREATE TABLE IF NOT EXISTS modules (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    path TEXT UNIQUE NOT NULL,              -- 'src/installer.ml'
    lines INTEGER NOT NULL DEFAULT 0,       -- line count
    intent TEXT,                            -- human-written purpose description
    last_analyzed TEXT,                     -- ISO 8601 timestamp
    has_mli BOOLEAN DEFAULT 0,              -- whether .mli exists
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_modules_lines ON modules(lines DESC);
CREATE INDEX IF NOT EXISTS idx_modules_no_intent ON modules(intent) WHERE intent IS NULL;

-- Functions
CREATE TABLE IF NOT EXISTS functions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    module_id INTEGER NOT NULL REFERENCES modules(id) ON DELETE CASCADE,
    name TEXT NOT NULL,                     -- 'install_node'
    signature TEXT,                         -- '?quiet:bool -> node_request -> (unit, R.msg) result'
    line_start INTEGER,
    line_end INTEGER,
    line_count INTEGER GENERATED ALWAYS AS (line_end - line_start + 1) STORED,
    exposed BOOLEAN DEFAULT 0,              -- appears in .mli
    is_alias BOOLEAN DEFAULT 0,             -- delegation alias (let f = Module.f)
    intent TEXT,                            -- human-written purpose description
    created_at TEXT DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(module_id, name)
);

CREATE INDEX IF NOT EXISTS idx_functions_module ON functions(module_id);
CREATE INDEX IF NOT EXISTS idx_functions_exposed ON functions(exposed);
CREATE INDEX IF NOT EXISTS idx_functions_no_intent ON functions(intent) WHERE intent IS NULL;
CREATE INDEX IF NOT EXISTS idx_functions_large ON functions(line_count DESC);

-- Call relationships (which functions call which)
CREATE TABLE IF NOT EXISTS calls (
    caller_id INTEGER NOT NULL REFERENCES functions(id) ON DELETE CASCADE,
    callee_id INTEGER NOT NULL REFERENCES functions(id) ON DELETE CASCADE,
    call_count INTEGER DEFAULT 1,           -- how many times caller calls callee
    PRIMARY KEY (caller_id, callee_id)
);

CREATE INDEX IF NOT EXISTS idx_calls_callee ON calls(callee_id);

-- Unsafe parameters (type safety tracking)
CREATE TABLE IF NOT EXISTS unsafe_params (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    function_id INTEGER NOT NULL REFERENCES functions(id) ON DELETE CASCADE,
    param_name TEXT NOT NULL,               -- 'instance'
    current_type TEXT NOT NULL,             -- 'string'
    target_type TEXT,                       -- 'Instance_name.t'
    fixed BOOLEAN DEFAULT 0,
    fixed_at TEXT,
    github_issue INTEGER,                   -- tracking issue number
    UNIQUE(function_id, param_name)
);

CREATE INDEX IF NOT EXISTS idx_unsafe_unfixed ON unsafe_params(fixed) WHERE fixed = 0;

-- Test coverage tracking
CREATE TABLE IF NOT EXISTS coverage (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    function_id INTEGER NOT NULL REFERENCES functions(id) ON DELETE CASCADE,
    covered_lines INTEGER NOT NULL DEFAULT 0,
    total_lines INTEGER NOT NULL DEFAULT 0,
    percentage REAL GENERATED ALWAYS AS (
        CASE WHEN total_lines > 0 THEN (covered_lines * 100.0 / total_lines) ELSE 0 END
    ) STORED,
    recorded_at TEXT DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(function_id, recorded_at)
);

CREATE INDEX IF NOT EXISTS idx_coverage_low ON coverage(percentage) WHERE percentage < 50;

-- Gardening tasks (links to GitHub issues)
CREATE TABLE IF NOT EXISTS gardening_tasks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    github_issue INTEGER UNIQUE,            -- GitHub issue number
    category TEXT NOT NULL,                 -- 'split-file', 'type-safety', 'coverage', etc.
    title TEXT,
    target_module_id INTEGER REFERENCES modules(id) ON DELETE SET NULL,
    target_function_id INTEGER REFERENCES functions(id) ON DELETE SET NULL,
    status TEXT DEFAULT 'open',             -- 'open', 'in_progress', 'done'
    created_at TEXT DEFAULT CURRENT_TIMESTAMP,
    completed_at TEXT
);

CREATE INDEX IF NOT EXISTS idx_tasks_status ON gardening_tasks(status);
CREATE INDEX IF NOT EXISTS idx_tasks_category ON gardening_tasks(category);

-- Gardening log (history of completed work)
CREATE TABLE IF NOT EXISTS gardening_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    date TEXT NOT NULL,
    contributor TEXT,
    category TEXT NOT NULL,
    description TEXT NOT NULL,
    pr_number INTEGER,
    issue_number INTEGER,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
);

-- Types (record, variant, abstract, alias)
CREATE TABLE IF NOT EXISTS types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    module_id INTEGER NOT NULL REFERENCES modules(id) ON DELETE CASCADE,
    name TEXT NOT NULL,                     -- 'node_request'
    kind TEXT NOT NULL,                     -- 'record', 'variant', 'abstract', 'alias', 'open'
    line_start INTEGER,
    line_end INTEGER,
    exposed BOOLEAN DEFAULT 0,              -- appears in .mli
    manifest TEXT,                          -- for aliases: the type it aliases
    intent TEXT,                            -- human-written purpose description
    created_at TEXT DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(module_id, name)
);

CREATE INDEX IF NOT EXISTS idx_types_module ON types(module_id);
CREATE INDEX IF NOT EXISTS idx_types_kind ON types(kind);
CREATE INDEX IF NOT EXISTS idx_types_exposed ON types(exposed);

-- Record fields
CREATE TABLE IF NOT EXISTS type_fields (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    type_id INTEGER NOT NULL REFERENCES types(id) ON DELETE CASCADE,
    field_name TEXT NOT NULL,               -- 'instance'
    field_type TEXT NOT NULL,               -- 'string'
    position INTEGER NOT NULL DEFAULT 0,    -- order within the record
    is_mutable BOOLEAN DEFAULT 0,           -- whether field is declared mutable
    UNIQUE(type_id, field_name)
);

CREATE INDEX IF NOT EXISTS idx_type_fields_type ON type_fields(type_id);
CREATE INDEX IF NOT EXISTS idx_type_fields_name ON type_fields(field_name);
CREATE INDEX IF NOT EXISTS idx_type_fields_ftype ON type_fields(field_type);

-- Variant constructors
CREATE TABLE IF NOT EXISTS type_constructors (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    type_id INTEGER NOT NULL REFERENCES types(id) ON DELETE CASCADE,
    constructor_name TEXT NOT NULL,          -- 'Genesis'
    position INTEGER NOT NULL DEFAULT 0,    -- order within the variant
    arg_types TEXT,                         -- comma-separated: 'string, int' or NULL for constant
    UNIQUE(type_id, constructor_name)
);

CREATE INDEX IF NOT EXISTS idx_type_constructors_type ON type_constructors(type_id);
CREATE INDEX IF NOT EXISTS idx_type_constructors_name ON type_constructors(constructor_name);

-- Mutable pattern usages (ref, :=, !, Atomic, mutable field access)
CREATE TABLE IF NOT EXISTS mutable_usages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    function_id INTEGER NOT NULL REFERENCES functions(id) ON DELETE CASCADE,
    kind TEXT NOT NULL,                     -- 'ref', 'ref_assign', 'ref_deref', 'atomic', 'mutable_field'
    line INTEGER NOT NULL,                  -- line number of usage
    UNIQUE(function_id, kind, line)
);

CREATE INDEX IF NOT EXISTS idx_mutable_usages_function ON mutable_usages(function_id);
CREATE INDEX IF NOT EXISTS idx_mutable_usages_kind ON mutable_usages(kind);

-- =============================================================================
-- Useful Views
-- =============================================================================

-- Large files needing attention
CREATE VIEW IF NOT EXISTS v_large_files AS
SELECT path, lines, intent, has_mli
FROM modules
WHERE lines > 500
ORDER BY lines DESC;

-- Large functions needing attention
CREATE VIEW IF NOT EXISTS v_large_functions AS
SELECT m.path, f.name, f.line_count, f.intent, f.exposed
FROM functions f
JOIN modules m ON f.module_id = m.id
WHERE f.line_count > 50
ORDER BY f.line_count DESC;

-- Functions without documentation
CREATE VIEW IF NOT EXISTS v_undocumented AS
SELECT m.path, f.name, f.signature, f.exposed
FROM functions f
JOIN modules m ON f.module_id = m.id
WHERE f.intent IS NULL AND f.exposed = 1
ORDER BY m.path, f.name;

-- Unsafe parameters to fix
CREATE VIEW IF NOT EXISTS v_unsafe_params AS
SELECT m.path, f.name, u.param_name, u.current_type, u.target_type, u.github_issue
FROM unsafe_params u
JOIN functions f ON u.function_id = f.id
JOIN modules m ON f.module_id = m.id
WHERE u.fixed = 0
ORDER BY m.path, f.name;

-- Low coverage functions
CREATE VIEW IF NOT EXISTS v_low_coverage AS
SELECT m.path, f.name, c.percentage, c.covered_lines, c.total_lines
FROM coverage c
JOIN functions f ON c.function_id = f.id
JOIN modules m ON f.module_id = m.id
WHERE c.percentage < 50
ORDER BY c.percentage ASC;

-- Most called functions (potential refactoring targets)
CREATE VIEW IF NOT EXISTS v_most_called AS
SELECT m.path, f.name, COUNT(c.caller_id) as caller_count
FROM functions f
JOIN modules m ON f.module_id = m.id
LEFT JOIN calls c ON f.id = c.callee_id
GROUP BY f.id
HAVING caller_count > 5
ORDER BY caller_count DESC;

-- Open gardening tasks by category
CREATE VIEW IF NOT EXISTS v_open_tasks AS
SELECT category, COUNT(*) as count, GROUP_CONCAT(github_issue) as issues
FROM gardening_tasks
WHERE status = 'open'
GROUP BY category
ORDER BY count DESC;

-- Types by field: find types containing a specific field name
CREATE VIEW IF NOT EXISTS v_type_fields AS
SELECT m.path, t.name as type_name, t.kind, tf.field_name, tf.field_type
FROM type_fields tf
JOIN types t ON tf.type_id = t.id
JOIN modules m ON t.module_id = m.id
ORDER BY m.path, t.name, tf.position;

-- Types by field type: find all types that contain a field of a given type
CREATE VIEW IF NOT EXISTS v_types_with_field_type AS
SELECT m.path, t.name as type_name, tf.field_name, tf.field_type
FROM type_fields tf
JOIN types t ON tf.type_id = t.id
JOIN modules m ON t.module_id = m.id
ORDER BY tf.field_type, m.path, t.name;

-- Variant constructors overview
CREATE VIEW IF NOT EXISTS v_variant_constructors AS
SELECT m.path, t.name as type_name, tc.constructor_name, tc.arg_types
FROM type_constructors tc
JOIN types t ON tc.type_id = t.id
JOIN modules m ON t.module_id = m.id
ORDER BY m.path, t.name, tc.position;

-- Mutable record fields
CREATE VIEW IF NOT EXISTS v_mutable_fields AS
SELECT m.path, t.name as type_name, tf.field_name, tf.field_type
FROM type_fields tf
JOIN types t ON tf.type_id = t.id
JOIN modules m ON t.module_id = m.id
WHERE tf.is_mutable = 1
ORDER BY m.path, t.name, tf.position;

-- Functions using mutable patterns
CREATE VIEW IF NOT EXISTS v_mutable_functions AS
SELECT m.path, f.name, mu.kind, COUNT(*) as usage_count
FROM mutable_usages mu
JOIN functions f ON mu.function_id = f.id
JOIN modules m ON f.module_id = m.id
GROUP BY f.id, mu.kind
ORDER BY m.path, f.name, mu.kind;

-- =============================================================================
-- Sample Queries (for reference)
-- =============================================================================

-- Find all functions that take a raw string 'instance' parameter:
-- SELECT * FROM v_unsafe_params WHERE param_name = 'instance';

-- Find functions called by many others (coupling hotspots):
-- SELECT * FROM v_most_called LIMIT 10;

-- Get gardening progress stats:
-- SELECT category,
--        SUM(CASE WHEN status = 'done' THEN 1 ELSE 0 END) as done,
--        SUM(CASE WHEN status = 'open' THEN 1 ELSE 0 END) as open
-- FROM gardening_tasks GROUP BY category;

-- Find modules without any function documentation:
-- SELECT m.path, COUNT(f.id) as func_count, SUM(CASE WHEN f.intent IS NULL THEN 1 ELSE 0 END) as undoc
-- FROM modules m
-- JOIN functions f ON f.module_id = m.id
-- GROUP BY m.id
-- HAVING undoc = func_count;

-- Find types that have both a string field and an int field:
-- SELECT DISTINCT t.name, m.path FROM types t
-- JOIN type_fields tf1 ON t.id = tf1.type_id AND tf1.field_type = 'string'
-- JOIN type_fields tf2 ON t.id = tf2.type_id AND tf2.field_type = 'int'
-- JOIN modules m ON t.module_id = m.id;

-- Find all types containing a field named 'instance':
-- SELECT * FROM v_type_fields WHERE field_name = 'instance';

-- Find all record types with a field of type 'baker_node_mode':
-- SELECT * FROM v_types_with_field_type WHERE field_type LIKE '%baker_node_mode%';

-- Find types that aggregate string, int, and some page type:
-- SELECT t.name, m.path, GROUP_CONCAT(tf.field_name || ':' || tf.field_type, ', ') as fields
-- FROM types t
-- JOIN modules m ON t.module_id = m.id
-- JOIN type_fields tf ON t.id = tf.type_id
-- WHERE t.id IN (SELECT type_id FROM type_fields WHERE field_type = 'string')
--   AND t.id IN (SELECT type_id FROM type_fields WHERE field_type = 'int')
--   AND t.id IN (SELECT type_id FROM type_fields WHERE field_type LIKE '%page%')
-- GROUP BY t.id;
