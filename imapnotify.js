var child_process = require('child_process');

function getStdout(cmd) {
  var stdout = child_process.execSync(cmd);
  return stdout.toString().trim();
}

exports.host = "imap.gmail.com";
exports.port = 993;
exports.tls = true;
exports.tlsOptions = { "rejectUnauthorized": false };
exports.username = "matthew@matthewwynn.com";
exports.password = getStdout("pass Email/apppass | head -n 1");
exports.onNewMail = "mbsync com.matthewwynn.matthew";
exports.onNewMailPost = "/opt/mailnotify";
exports.boxes = [ "INBOX" ];
