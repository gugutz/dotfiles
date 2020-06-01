fvdSynchronizer.PremiumForShare = new function(){
  function isAuthorizedOnServer(cb) {
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "https://everhelper.me/shareforpremium/can.php");
    xhr.onload = function() {
      if(String(resp).indexOf('{') !== 0){
        cb(false);
      }else{
        var resp = JSON.parse(xhr.responseText);
        if(!resp.can){
          cb(false);
        }
        else{
          cb(true);
        }
      }
    };
    xhr.send(JSON.stringify({
      action: "user:authstate"
    }));
  }

  this.canDisplay = function(params, cb) {
    if(typeof params == "function"){
      cb = params;
      params = {};
    }
    var installTime = parseInt(fvdSynchronizer.Prefs.get("install.time"));
    // remove '&& false'
    if(new Date().getTime() - installTime < 3600 * 24 * 7 * 1000 && false){
      return cb(false);
    }

    isAuthorizedOnServer(function(authorized) {
      if(!authorized){
        return cb(false);
      }
      cb(true);
    });
  };
}();
