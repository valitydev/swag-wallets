#!/usr/bin/env node
'use strict';
var Path = require('path');

require('shelljs/global');
set('-e');

var TARGET_DIR = 'dist'
if (process.argv[2]) {
    TARGET_DIR = process.argv[2]
}

var APIS = ls('api')
if (process.argv[3]) {
    APIS = [process.argv[3]]
}

mkdir('-p', TARGET_DIR);
cp('-R', 'web/*', TARGET_DIR + '/');

APIS.forEach(function (api) {
    var basedir = 'api/' + api + '/spec/';
    var targetdir = TARGET_DIR + '/api/' + api;
    mkdir('-p', targetdir);
    exec('npm run swagger bundle -- --basedir ' + basedir + '        -o ' + targetdir + '/swagger.json');
    exec('npm run swagger bundle -- --basedir ' + basedir + ' --yaml -o ' + targetdir + '/swagger.yaml');
});

var SWAGGER_UI_DIST = Path.dirname(require.resolve('swagger-ui'));
rm('-rf', TARGET_DIR + '/swagger-ui/')
cp('-R', SWAGGER_UI_DIST, TARGET_DIR + '/swagger-ui/')
sed('-i', 'http://petstore.swagger.io/v2/swagger.json', '../swagger.json', TARGET_DIR + '/swagger-ui/index.html')
