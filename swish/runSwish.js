const spawn = require('cross-spawn');
//const child = spawn('ls');
const child = spawn('swish/go', {detached: true});
