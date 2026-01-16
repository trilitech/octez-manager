// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

// https://astro.build/config
export default defineConfig({
	site: 'https://octez-manager.tezos.com',
	base: '/',
	integrations: [
		starlight({
			title: 'Octez Manager',
			logo: {
				src: './public/favicon.png',
			},
			favicon: '/favicon.png',
			description: 'CLI and TUI for managing Tezos nodes, bakers, and DAL infrastructure',
			social: [
				{ icon: 'github', label: 'GitHub', href: 'https://github.com/trilitech/octez-manager' },
			],
			sidebar: [
				{
					label: 'Getting Started',
					items: [
						{ label: 'Introduction', slug: 'getting-started/introduction' },
						{ label: 'Installation', slug: 'getting-started/installation' },
						{ label: 'Using the TUI', slug: 'guides/tui-guide' },
					],
				},
				{
					label: 'Guides',
					items: [
						{ label: 'Node Setup', slug: 'guides/node-setup' },
						{ label: 'Baker Setup', slug: 'guides/baker-setup' },
						{ label: 'DAL Node Setup', slug: 'guides/dal-node-setup' },
					],
				},
				{
					label: 'Reference',
					items: [
						{ label: 'CLI Reference', slug: 'reference/cli' },
					],
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
