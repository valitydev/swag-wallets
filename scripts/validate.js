#!/usr/bin/env node
'use strict';
const path = require('path');
const swagger = require('@apidevtools/swagger-parser');
const { exit } = require('shelljs');

const destDir = 'dist/api';

function validateSpec(specFilename) {
    swagger.validate(specFilename, function (err) {
        if (err) {
            console.error(`${specFilename}:`);
            console.error(err.message);
            exit(1);
        }
    });
}

validateSpec(path.join(destDir, 'wallet/swagger.yaml'));
validateSpec(path.join(destDir, 'payres/swagger.yaml'));
