---
language: typescript
description: Good patterns and antipatterns for TypeScript — nominal types, exhaustiveness, and boundary safety.
version: 1.0.0
---

# TypeScript Patterns

## Good Patterns

**Branded/nominal types for domain values**
```ts
type UserId = string & { readonly _brand: 'UserId' }
const toUserId = (s: string): UserId => s as UserId
```
Prevents accidental mixing of structurally identical primitives.

**Discriminated unions over nullable optional fields**
```ts
type Result<T, E> = { ok: true; value: T } | { ok: false; error: E }
```
Exhaustive `switch` on the discriminant field gives compile-time completeness.

**`unknown` not `any` at boundaries**
Parse external data with a validator (zod, io-ts) that narrows `unknown` to a typed value. Never widen to `any`.

**`readonly` arrays and properties**
`readonly string[]` and `Readonly<Config>` prevent accidental mutation. Default to immutable; opt into mutation deliberately.

**`never` for exhaustiveness checks**
```ts
function assertNever(x: never): never { throw new Error('Unhandled case') }
```
Place in the `default` branch of a discriminated union switch to get a type error when a new variant is unhandled.

**`strict: true` in tsconfig**
Enables `noImplicitAny`, `strictNullChecks`, `strictFunctionTypes`, and more. Non-strict TypeScript is a different (weaker) language.

**Union types over string enums for closed sets**
`type Direction = 'north' | 'south' | 'east' | 'west'` is narrower and more ergonomic than `enum Direction { North = 'north', ... }`.

---

## Antipatterns

**`any`**
Silences the type system entirely. Use `unknown` and narrow, or define the actual type.

**Non-null assertion `!`**
`user!.name` defers a potential null crash to runtime. Use optional chaining + explicit fallback or a type guard instead.

**Optional chaining on required fields**
`user?.name` where `user` is always present hides a design flaw. If it can be absent, make that explicit in the type.

**`object` or `{}` as a type**
Too wide to be useful. Define the shape or use `Record<string, unknown>`.

**`// @ts-ignore` without explanation**
If the type error is wrong, explain why in the comment. If it is right, fix it.

**Mutable arrays where `readonly` suffices**
`string[]` as a function parameter invites the callee to mutate the caller's data. Use `readonly string[]`.

**`null` AND `undefined` for absence**
Pick one. TypeScript already has two absence values; mixing them makes narrowing needlessly complex. Prefer `undefined` (it is the default absent value in JS); use `null` only at JSON/API boundaries.

**Throwing bare strings**
`throw 'something went wrong'` loses stack traces and type information. `throw new Error('...')` or a typed error class.
