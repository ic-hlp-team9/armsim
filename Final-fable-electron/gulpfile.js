const gulp = require('gulp');

    // Create an electron-connect server to enable reloading
    const electron = require('electron-connect').server.create('ElectronMain.js');

    gulp.task('refresh', ()=>{
      console.log('Restarting client...');
      electron.restart();
    });

    gulp.task('reload', ()=>{
      console.log('Reloading client...');
      electron.reload();
    });

    gulp.task('start', ()=>{
      // Start the Electron application renderer and server tasks
      electron.start('ElectronMain.js');
      //Watch js files and restart Electron if they change
      gulp.watch(['./dist/umd/*.js'], ['refresh']);
      //Watch css, html files, but only reload (no restart necessary)
      gulp.watch(['./dist/umd/*.css', './dist/index.html'], ['reload']);
    });