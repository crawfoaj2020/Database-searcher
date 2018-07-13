// Modules to control application life and create native browser window
const {app, BrowserWindow} = require('electron');
const ipcRenderer = require('electron').ipcRenderer;
var kill = require('tree-kill');
var path = require('path')
const spawn = require('cross-spawn');
// const child = spawn('./go');
const child = spawn('./go', {detached: true});

let mainWindow

function createWindow () {
  // Create the browser window.
  
  mainWindow = new BrowserWindow({show: false,
				  icon: path.join(__dirname, 'assets/icons/png/64x64.png'),
				  backgroundColor: '#4e284c'})
  mainWindow.maximize()
  mainWindow.show()

  mainWindow.loadURL('http://localhost:54321');

  // Emitted when the window is closed.
  mainWindow.on('closed', function () {
    mainWindow = null;
  })
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', createWindow)

// Quit when all windows are closed.
app.on('window-all-closed', function () {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform !== 'darwin') {
    app.quit();
    const child = require('child_process')
    const killed = child.exec('taskkill /f /im swish.exe');
  }
})

app.on('activate', function () {
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (mainWindow === null) {
    createWindow()
  }
})

