const gulp = require('gulp');
const autoprefixer = require('gulp-autoprefixer');
const sass = require('gulp-sass');
const exec = require('gulp-exec');
const clean = require('gulp-clean');
const sequence = require('gulp-sequence');
const cleanCSS = require('gulp-clean-css');

gulp.task('default',
	sequence(
		'clean',
		'css',
		'public',
		'elm'
	));

/*
 * Clean up existing dist
 */
gulp.task('clean', () =>
	gulp.src('dist', {read: false})
		.pipe(clean()));

/*
 * Compile Sass to CSS and autoprefix it
 */
gulp.task('css', () =>
	gulp.src('src/ui.sass')
		.pipe(sass().on('error', sass.logError))
		.pipe(autoprefixer({
			browsers: ['last 2 versions'],
		}))
		.pipe(cleanCSS())
		.pipe(gulp.dest('dist/css')));

/*
 * Copy files to dist
 */
gulp.task('public', () =>
	gulp.src('public/**/*')
		.pipe(gulp.dest('dist')));

/*
 * Compile elm files
 */
gulp.task('elm', () =>
	gulp.src('./elm-package.json')
		.pipe(exec('elm make src/Main.elm --output dist/js/elm.js'))
		.pipe(exec.reporter()));
