#!/usr/bin/env node
'use strict';

// Usage: node run-effectiveness.js totalSeeds multiplier addend
// Run from tests/ directory.

if (process.argv.length !== 5) {
  console.error('Usage: node run-effectiveness.js totalSeeds multiplier addend');
  process.exit(1);
}

const totalSeeds = parseInt(process.argv[2], 10);
const multiplier = parseInt(process.argv[3], 10);
const addend = parseInt(process.argv[4], 10);

const runner = require('./effectiveness-runner.js');
const Elm = runner.Elm;
if (!Elm || !Elm.EffectivenessMain || !Elm.EffectivenessMain.init) {
  console.error('Elm.EffectivenessMain.init not found. Build with: elm make src/EffectivenessMain.elm --output effectiveness-runner.js');
  process.exit(1);
}

Elm.EffectivenessMain.init({
  flags: { totalSeeds, multiplier, addend }
});
