var gulp = require("gulp");
var purescript = require("gulp-purescript");
var run = require("gulp-run");
var del = require("del");

var sources = [
  "src/**/*.purs",
  "test/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs",
];

var distFiles = [
  "index.html",
  "*.css",
  "output/app.js"
];

var compile = gulp.series(cleanAppJs, compileOnly);
var bundle = gulp.series(cleanAppJs, compile, bundleOnly);
var test = gulp.series(compile, testOnly);
var dist =  gulp.series(bundle, copyOnly, copyDemoSources);
var testAndDist =  gulp.series(compile, testOnly, bundleOnly, copyOnly, copyDemoSources);


gulp.task("clean", clean);
gulp.task("compile", compile);
gulp.task("test", test);
gulp.task("bundle", bundle);
gulp.task("dist", dist);
gulp.task("default", testAndDist);


function clean () {
    return del([ 'output', 'bower_components', 'node_modules', 'dist' ]);
}

function cleanAppJs () {
    return del([ 'output/app.js' ]);
}

function compileOnly () {
    return purescript.compile({ src: sources });
}

function bundleOnly () {
    return purescript.bundle(
        { src: "output/**/*.js"
          , module: "Main"
          , main: "Main"
          , output: "output/app.js" });
};

function testOnly () {
    return purescript.bundle({ src: "output/**/*.js", main: "Test.Main" })
        .pipe(run("node"));
}

function copyOnly () {
    return gulp.src(distFiles)
        .pipe(gulp.dest("dist"));
}

function copyDemoSources () {
    return gulp.src("src/Demo/*.purs")
        .pipe(gulp.dest("dist/src/Demo/"));
};
