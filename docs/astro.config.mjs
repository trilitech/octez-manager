// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

// https://astro.build/config
export default defineConfig({
	site: 'https://mathiasbourgoin.github.io',
	base: '/octez-manager',
	integrations: [
		starlight({
			title: 'Octez Manager',
			logo: {
				src: './public/favicon.png',
			},
			favicon: '/favicon.png',
			description: 'CLI and TUI for managing Tezos nodes, bakers, and DAL infrastructure',
			social: [
				{ icon: 'github', label: 'GitHub', href: 'https://github.com/mathiasbourgoin/octez-manager' },
			],
			sidebar: [
				{
					label: 'Getting Started',
					items: [
						{ label: 'Introduction', slug: 'getting-started/introduction' },
						{ label: 'Installation', slug: 'getting-started/installation' },
						{ label: 'Quick Start', slug: 'getting-started/quick-start' },
					],
				},
				{
					label: 'Guides',
					items: [
						{ label: 'Setting Up a Node', slug: 'guides/node-setup' },
						{ label: 'Becoming a Baker', slug: 'guides/baker-setup' },
						{ label: 'DAL Node Setup', slug: 'guides/dal-node-setup' },
						{ label: 'Using the TUI', slug: 'guides/tui-guide' },
					],
				},
				{
					label: 'Reference',
					autogenerate: { directory: 'reference' },
				},
			],
			customCss: ['./src/styles/custom.css'],
			head: [
				{
					tag: 'meta',
					attrs: {
						name: 'theme-color',
						content: '#0D61FF',
					},
				},
			],
		}),
	],
});
