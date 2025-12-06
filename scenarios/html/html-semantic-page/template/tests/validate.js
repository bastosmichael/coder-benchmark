
const fs = require('fs');
const path = require('path');

const filePath = path.join(__dirname, '../src/index.html');
const html = fs.readFileSync(filePath, 'utf8');

function assert(condition, msg) {
    if (!condition) {
        console.error('FAILURE: ' + msg);
        process.exit(1);
    }
}

// Basic Regex Checks
assert(/<!DOCTYPE html>/i.test(html), 'Missing DOCTYPE');
assert(/<html[^>]*lang=['"]en['"]/.test(html) || /<html[^>]*lang=/.test(html), 'Missing html lang attribute');
assert(/<header>[\s\S]*<\/header>/.test(html), 'Missing <header>');
assert(/<h1>Brew Haven<\/h1>/i.test(html), 'Missing h1 title');
assert(/<nav>[\s\S]*<ul>/.test(html), 'Missing nav or ul');
assert(/<main>/.test(html), 'Missing <main>');
assert(/<section[^>]*id="hero"/.test(html), 'Missing hero section');
assert(/<section[^>]*id="features"/.test(html), 'Missing features section');
assert(/<section[^>]*id="menu"/.test(html), 'Missing menu section');
assert(/<table/.test(html), 'Missing table');
assert(/<footer>/.test(html), 'Missing footer');

console.log('All tests passed');
