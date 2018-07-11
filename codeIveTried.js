//No error, loads blank page
const exec = require('child_process').exec;
  function execute(command, callback) {
    exec(command, (error, stdout, stderr) => { 
        callback(stdout); 
    });
  };
  // call the function
  execute('./go', (output) => {
    console.log(output);
  });


//Error: Invalid or unexpected token
var subpy = require('child_process').execSync('./go');

//Error: Invalid or unexpected token
var subpy = require('child_process').exec('./go');


//Current thought: Windows shell different from linux shell
