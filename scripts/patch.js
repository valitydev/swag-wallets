#!/usr/bin/env node
'use strict';
const fs = require('fs');
const path = require('path');
const yaml = require('yaml');
const patcher = require('yaml-diff-patch');
const shell = require('shelljs');

function patchSpec(specFilename, patchFilename, destDir) {

    const sourceDoc = fs.readFileSync(specFilename, 'utf8');
    const patch = yaml.parse(fs.readFileSync(patchFilename, 'utf8'));
    const patchedDoc = patcher.yamlPatch(sourceDoc, patch);
    
    shell.mkdir("-p", destDir);
    fs.writeFileSync(path.join(destDir, 'swagger.yaml'), patchedDoc);
    fs.writeFileSync(path.join(destDir, 'swagger.json'), JSON.stringify(yaml.parse(patchedDoc), null, '  '));    

}

const specBaseDir = 'spec/api';
const destDir = 'dist/api';

patchSpec(
    path.join(specBaseDir, 'wallet/swagger.yaml'),
    'patches/wallet.yaml',
    path.join(destDir, 'wallet')
);

patchSpec(
    path.join(specBaseDir, 'payres/swagger.yaml'),
    'patches/payres.yaml',
    path.join(destDir, 'payres')
);
