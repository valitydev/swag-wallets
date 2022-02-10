const gulp = require('gulp');
const gulpConnect = require('gulp-connect');
const exec = require('child_process').exec;
const portfinder = require('portfinder');

const DIST_DIR = 'dist';
const SPEC_DIR = 'spec';
const PATCH_DIR = 'patches';

function patch(cb) {
  return exec('npm run patch');
}

function watch(cb) {
  gulp.watch(
    [
      `${SPEC_DIR}/**/*`,
      `${PATCH_DIR}/**/*`,
      'web/**/*'
    ],
    gulp.series(
      patch,
      function () {
        return gulp.src(DIST_DIR).pipe(gulpConnect.reload())
      }
    )
  );
}

function server(cb) {
  portfinder.getPort({ port: 3000 }, function (err, port) {
    gulpConnect.server({
      root: [DIST_DIR],
      livereload: true,
      port: port
    });
  });
}

exports.patch = patch;
exports.watch = watch;
exports.serve = gulp.parallel(patch, watch, server);
