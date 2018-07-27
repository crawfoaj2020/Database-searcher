// Modules to control application life and create native browser window
const {app, BrowserWindow} = require('electron');
const ipcRenderer = require('electron').ipcRenderer;
var kill = require('tree-kill');
var path = require('path')
const spawn = require('cross-spawn');
//const child = spawn('./go');
const child = spawn('swish/go', {detached: true});

let mainWindow

//Only allow one instance
const isSecondInstance = app.makeSingleInstance((commandLine, workingDirectory) => {
    // Someone tried to run a second instance, we should focus our window.
    if (mainWindow) {
        if (mainWindow.isMinimized()) mainWindow.restore()
        mainWindow.focus()
    }
})
if (isSecondInstance) {
    app.quit()
}



function createWindow () {
  // Create the browser window.
  
  mainWindow = new BrowserWindow({show: false,
				  icon: path.join(__dirname, 'assets/icons/png/64x64.png'),
				  backgroundColor: '#4e284c'})
  mainWindow.maximize()
  mainWindow.show()

  try{
    mainWindow.loadURL('http://127.0.0.1:54321');
  }
  catch(e){
    mainWindow.loadURL('http://localhost:54321/app/saved?type=database&sql=&limit=100&offset=0&flag=An+Error+occured+so+returned+home+please+see+debug+tab+for+more+information')
  }
    

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
    const childProc = require('child_process')
    const killed = childProc.exec('taskkill /f /im swish.exe');
  }
})

app.on('activate', function () {
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (mainWindow === null) {
    createWindow()
  }
})
