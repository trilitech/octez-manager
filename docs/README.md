# Octez Manager Documentation

This directory contains the documentation website for Octez Manager, built with [Starlight](https://starlight.astro.build).

## Development

```bash
cd docs
npm install
npm run dev
```

The dev server runs at `localhost:4321`.

## Build

```bash
npm run build
```

Output is in `./dist/`.

## Structure

- `src/content/docs/` - Documentation pages (Markdown/MDX)
- `src/assets/` - Images and other assets
- `public/` - Static files (favicon, etc.)
- `astro.config.mjs` - Site configuration
