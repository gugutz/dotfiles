var Profile = function(log) {

  this._durations = {};
  this._startTimes = {};
  this._log = log;

};

Profile.prototype.start = function(action) {  
  if(this._startTimes[action]){
    this.end(action);
  }
  
  this._startTimes[action] = new Date().getTime();
};

Profile.prototype.end = function(action) {
  if (!this._durations[action]) {
    this._durations[action] = 0;
  }
  this._durations[action] += new Date().getTime() - this._startTimes[action];
  this._startTimes[action] = 0;
};

Profile.prototype.getReport = function() {
  var lines = [];
  for (var action in this._durations ) {
    lines.push(action + ": " + (Math.round(this._durations[action] / 100) / 10) + "s");
  }

  return lines.join("\n");
};

Profile.prototype.writeReport = function() {
  var report = this.getReport();
  this._log.add("profile", report);
};

var Log = function(eRequest) {

  var self = this;

  this.messages = [];
  this._profile = null;
  this._vars = {};

  var types = ["error", "info", "log", "debug"];

  types.forEach(function(type) {

    self[type] = function() {
      var args = Array.prototype.slice.apply(arguments);
      args.unshift(type);

      self.add.apply(self, args);
    };

  });
  
  this.__defineGetter__("profile", function(){
    if(!this._profile){
      this._profile = new Profile(this);
    }
    return this._profile;
  });

};

Log.prototype.add = function() {

  var args = Array.prototype.slice.call(arguments);

  var type = args.shift();

  var message = "";

  for (var i = 0; i != args.length; i++) {
    message += " ";

    if ( typeof args[i] == "object") {
      message += JSON.stringify(args[i]);
    } else {
      message += args[i];
    }
  }

  this.messages.push({
    date : new Date(),
    type : type,
    message : message
  });

};

Log.prototype.inc = function(name, by){
  by = by || 1;
  
  if(!this._vars[name]){
    this._vars[name] = 0;
  }
  
  this._vars[name] += by;
};

Log.prototype.toString = function() {

  var lines = [];

  this.messages.forEach(function(msg) {

    var line = msg.date.toString() + ": " + msg.type + ", " + msg.message;

    lines.push(line);

  });
  
  lines.push("----------------");
  
  if(this._profile){
    lines.push("Profiling:");
    lines.push(this._profile.getReport());
  }
  lines.push("Vars:");
  for(var k in this._vars){
    lines.push(k + "=" + this._vars[k]);
  }
  return lines.join("\n");

};