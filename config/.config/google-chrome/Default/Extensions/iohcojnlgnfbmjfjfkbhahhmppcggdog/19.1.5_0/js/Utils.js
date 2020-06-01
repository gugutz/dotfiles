function _b( v ){
  if( typeof v == "boolean" ){
    return v;
  }

  if( v == "true" ){
    return true;
  }

  return false;
}

function _isb( v ){
  if( typeof v == "boolean" ){
    return true;
  }

  if( v == "true" || v == "false" ){
    return true;
  }

  return false;
}

function _r( v ){

  if( _isb( v ) ){
    return _b(v);
  }
  return v;

}

var SimpleBench = function( start ){

  if( typeof start == "undefined" ){
    start = true;
  }

  var starts = null;
  var called = 0;

  var dur = 0;

  this.start = function(){
    starts = new Date().getTime();
    called++;
  }

  this.inc = function(){

    if( starts ){
      dur += new Date().getTime() - starts;
      starts = null;
    }

  }

  this.end = function( prefix ){

    this.inc();

    prefix = prefix || "";

    console.log( prefix, dur/1000, "Called: ", called, "times");
    if( called > 1 ){
      console.log( "Median: ", dur/1000/called );
    }

  }

  if( start ){
    this.start();
  }

}



function EventEmitter(){
  var callbacks = [];

  this.addListener = function( listener ){

    callbacks.push( listener );

  }

  this.removeListener = function( listener ){

    var index = callbacks.indexOf( listener );

    if( index != -1 ){
      callbacks.splice( index, 1 );
    }

  }

  this.callListeners = function(){

    var args = arguments;

    var toRemove = [];

    callbacks.forEach(function( callback ){

      try{
        callback.apply( window, args );
      }
      catch( ex ){
        toRemove.push( callback );
      }

    });

    toRemove.forEach(function( callback ){

      var index = callbacks.indexOf( callback );
      if( index > -1 ){
        callbacks.splice( index, 1 );
      }

    });
  }
};

// extends


(function(){

  var Utils = function(){

  }

  Utils.prototype = {

    _isVersionChanged: false,

    getOptionsPagesOpenedTabsIds: function(cb) {
      var url = chrome.runtime.getURL("/options.html");
      chrome.tabs.query({
        url: url
      }, function(tabs) {
        // move active tab to top
        tabs.sort(function(t1, t2) {
          if(t1.active) {
            return 1;
          }
          if(t2.active) {
            return -1;
          }
          return 0;
        });
        var tabsIds = tabs.map(function(tab) {return tab.id});
        cb(null, tabsIds);
      });
    },

    browserLocaleIs: function(locale) {
      if(typeof navigator.language !== "string") {
        return false;
      }
      return new RegExp("\\b" + locale + "\\b", "i").test(navigator.language);
    },

    generateGUID: function() {
      var chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghiklmnopqrstuvwxyz";
      var string_length = 32;
      var randomstring = '';
      for (var i=0; i<string_length; i++) {
        var rnum = Math.floor(Math.random() * chars.length);
        randomstring += chars.substring(rnum,rnum+1);
      }
      return randomstring;
    },

    splitString: function(str, len) {
      var ret = [];
      for (var offset = 0, strLen = str.length; offset < strLen; offset += len) {
        ret.push(str.slice(offset, len + offset));
      }
      return ret;
    },

    dataURItoBlob: function(dataURI) {
      var tmp = dataURI.split(',');

      tmp[0] = tmp[0].replace("data:", "").replace(";base64", "");

        var binary = atob(tmp[1]);
        var array = [];
        for(var i = 0; i < binary.length; i++) {
            array.push(binary.charCodeAt(i));
        }
        return new Blob([new Uint8Array(array)], {type: tmp[0]});
    },

    getSecondsCountAfterInstall: function(){

      var installTime = fvdSynchronizer.Prefs.get( "install.time" );
      if( !installTime ){
        fvdSynchronizer.Prefs.set( "install.time", new Date().getTime() );
        return 0;
      }
      else{
        try{
          installTime = parseInt( installTime );
        }
        catch( ex ){

        }

        return (new Date().getTime() - installTime)/1000;
      }

    },

    isVersionChanged: function(){

      if( this._isVersionChanged ){
        return this._isVersionChanged;
      }

      var app = chrome.app.getDetails();

      if( fvdSynchronizer.Prefs.get( "last_run_version" ) != app.version ){
        this._isVersionChanged = true;
        fvdSynchronizer.Prefs.set( "last_run_version", app.version );
      }

      return this._isVersionChanged;

    },

    containsAllAscii: function(str){
      return  /^[\000-\177]*$/.test(str);
    },

    objectClone: function( object ){

      return JSON.parse( JSON.stringify( object ) );

    },

    objectsMerge: function( o1, o2 ){

      for( var k in o2 ){
        o1[k] = o2[k];
      }

    },

    objectsDiffFields: function( o1, o2, checkFields ){

      var fields = [];

      checkFields.forEach( function( field ){

        if( o1[field] && o1[field].trim ){
          o1[field] = o1[field].trim();
        }

        if( o2[field] && o2[field].trim ){
          o2[field] = o2[field].trim();
        }

        if( o1[field] != o2[field] ){
          fields.push( field );
        }

      } );

      return fields;

    },

    arrayUnique: function( a ){

      var result = [];

      for( var i = 0; i != a.length; i++ ){
        var v = a[i];
        if( result.indexOf( v ) == -1 ){
          result.push( v );
        }
      }

      return result;

    },

    arrayFilter: function( a ){

      var result = [];

      for( var i = 0; i != a.length; i++ ){
        var v = a[i];
        if( v ){
          result.push( v );
        }
      }

      return result;

    },

    getActiveTab: function( callback ){
      chrome.tabs.query( {
        active: true
      }, function( tabs ){
        if( tabs.length == 0 ){
          callback( null );
        }
        else{
          callback( tabs[0] );
        }
      } );
    },

    arrayDiff: function( a1, a2 ){
      return a1.filter(function(i) {return !(a2.indexOf(i) > -1);});
    },

    urlToCompareForm: function( url ){
      if(!url){
        return "";
      }

      url = url.toLowerCase();
      url = url.replace( /https?:\/\//i, "" );
      url = url.replace( /^www\./i, "" );
      url = url.replace( /\/+$/i, "" );

      return url;
    },

    isValidUrl: function( url ){

      if( url.indexOf( "file:///" ) === 0 ){
        return true;
      }

      try{
        var parsed = this.parseUrl( url );
        if( !parsed.host ){
          return false;
        }

        /*
        if( parsed.host.indexOf(".") == -1 ){
          return false;
        }
        */

        if( parsed.host.length < 2 ){
          return false;
        }

        return true;
      }
      catch( ex ){
        return false;
      }
    },

    validateText: function( type, text ){
      switch( type ){
        case "email":

            var re = /^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
          return re.test( text );

        break;
      }
    },

    isIdenticalUrls: function( url1, url2 ){
      url1 = this.urlToCompareForm( url1 );
      url2 = this.urlToCompareForm( url2 );

      return url1 == url2;
    },

    isIdenticalHosts: function( host1, host2 ){
      host1 = this.urlToCompareForm( host1 );
      host2 = this.urlToCompareForm( host2 );

      return host1 == host2;
    },

    buildURL: function( parsed ){

      var string = "";

      string += parsed.scheme;
      string += "://";
      if( parsed.user && parsed.pass ){
        string += parsed.user + ":" + parsed.pass + "@";
      }
      string += parsed.host;

      if( !parsed.path ){
        parsed.path = "/";
      }

      string += parsed.path;

      if( parsed.query ){
        string += "?" + parsed.query;
      }

      if( parsed.fragment ){
        string += "#" + parsed.fragment;
      }

      return string;

    },


    parseUrl: function(str, component){

        // Parse a URL and return its components
        //
        // version: 1109.2015
        // discuss at: http://phpjs.org/functions/parse_url
        // +      original by: Steven Levithan (http://blog.stevenlevithan.com)
        // + reimplemented by: Brett Zamir (http://brett-zamir.me)
        // + input by: Lorenzo Pisani
        // + input by: Tony
        // + improved by: Brett Zamir (http://brett-zamir.me)
        // %          note: Based on http://stevenlevithan.com/demo/parseuri/js/assets/parseuri.js
        // %          note: blog post at http://blog.stevenlevithan.com/archives/parseuri
        // %          note: demo at http://stevenlevithan.com/demo/parseuri/js/assets/parseuri.js
        // %          note: Does not replace invalid characters with '_' as in PHP, nor does it return false with
        // %          note: a seriously malformed URL.
        // %          note: Besides function name, is essentially the same as parseUri as well as our allowing
        // %          note: an extra slash after the scheme/protocol (to allow file:/// as in PHP)
        // *     example 1: parse_url('http://username:password@hostname/path?arg=value#anchor');
        // *     returns 1: {scheme: 'http', host: 'hostname', user: 'username', pass: 'password', path: '/path', query: 'arg=value', fragment: 'anchor'}
        var key = ['source', 'scheme', 'authority', 'userInfo', 'user', 'pass', 'host', 'port', 'relative', 'path', 'directory', 'file', 'query', 'fragment'], ini = (this.php_js && this.php_js.ini) ||
        {}, mode = (ini['phpjs.parse_url.mode'] &&
        ini['phpjs.parse_url.mode'].local_value) ||
        'php', parser = {
            php: /^(?:([^:\/?#]+):)?(?:\/\/()(?:(?:()(?:([^:@]*):?([^:@]*))?@)?([^:\/?#]*)(?::(\d*))?))?()(?:(()(?:(?:[^?#\/]*\/)*)()(?:[^?#]*))(?:\?([^#]*))?(?:#(.*))?)/,
            strict: /^(?:([^:\/?#]+):)?(?:\/\/((?:(([^:@]*):?([^:@]*))?@)?([^:\/?#]*)(?::(\d*))?))?((((?:[^?#\/]*\/)*)([^?#]*))(?:\?([^#]*))?(?:#(.*))?)/,
            loose: /^(?:(?![^:@]+:[^:@\/]*@)([^:\/?#.]+):)?(?:\/\/\/?)?((?:(([^:@]*):?([^:@]*))?@)?([^:\/?#]*)(?::(\d*))?)(((\/(?:[^?#](?![^?#\/]*\.[^?#\/.]+(?:[?#]|$)))*\/?)?([^?#\/]*))(?:\?([^#]*))?(?:#(.*))?)/ // Added one optional slash to post-scheme to catch file:/// (should restrict this)
        };

        var m = parser[mode].exec(str), uri = {}, i = 14;
        while (i--) {
            if (m[i]) {
                uri[key[i]] = m[i];
            }
        }

        if (component) {
            return uri[component.replace('PHP_URL_', '').toLowerCase()];
        }
        if (mode !== 'php') {
            var name = (ini['phpjs.parse_url.queryKey'] &&
            ini['phpjs.parse_url.queryKey'].local_value) ||
            'queryKey';
            parser = /(?:^|&)([^&=]*)=?([^&]*)/g;
            uri[name] = {};
            uri[key[12]].replace(parser, function($0, $1, $2){
                if ($1) {
                    uri[name][$1] = $2;
                }
            });
        }
        delete uri.source;
        return uri;
    },

    prepareUrlToCompare: function( url ){
      url = url.toLowerCase();
      // remove http from sign
      url = url.replace("http://", "");
      url = url.replace("https://", "");
      // remove www from sign
      url = url.replace( "www.", "" );
      // remove and "/"
      if( url.charAt( url.length - 1 ) == "/" ){
        url = url.substring( 0, url.length - 1 );
      }

      return url;
    },

    isUrlsEqual: function( url1, url2 ){
      return this.prepareUrlToCompare( url1 ) == this.prepareUrlToCompare( url2 );
    },

    isVisibleElem: function( elem ){

      var viewportHeight = window.innerHeight;
      var currentTopStart = document.body.scrollTop;
      var currentTopEnd = currentTopStart + viewportHeight;

      var elemOffset = this.getOffset( elem );

      if( elemOffset.top > currentTopStart && elemOffset.top + elem.offsetHeight < currentTopEnd ){
        return true;
      }

      return false;

    },

    refreshTabsByUrl: function( url ){

      chrome.tabs.query({
        url: url
      }, function( tabs ){

        tabs.forEach(function( tab ){
          chrome.tabs.reload( tab.id );
        });

      });

    },

    scrollToElem: function( elem ){

      var viewportHeight = window.innerHeight;
      var currentTopStart = document.body.scrollTop;
      var currentTopEnd = currentTopStart + viewportHeight;

      var elemOffset = this.getOffset( elem );

      if( elemOffset.top > currentTopStart && elemOffset.top + elem.offsetHeight < currentTopEnd ){
        return; // no need scroll
      }

      var scrollAmount = 0;
      if( elemOffset.top < currentTopStart ){
        scrollAmount = elemOffset.top;
      }
      else{
        scrollAmount = elemOffset.top + elem.offsetHeight - viewportHeight;
      }

      document.body.scrollTop = scrollAmount;

    },

    getOffset: function( obj ) {
      var curleft = curtop = 0;
      if (obj.offsetParent) {
        do {
          curleft += obj.offsetLeft;
          curtop += obj.offsetTop;
        }
        while(obj = obj.offsetParent);
      }



      return {
        "left": curleft,
        "top": curtop
      };
    },

    copyToClipboard: function( text ){
      var bg = chrome.extension.getBackgroundPage();

      var clipboardholder = bg.document.getElementById("clipboardholder");
      clipboardholder.value = text;
      clipboardholder.select();
      bg.document.execCommand("Copy");
    },

    ucfirst: function( str ){
      var firstLetter = str.slice(0,1);
      return firstLetter.toUpperCase() + str.substring(1);
    },

    cropLength: function( str, len ){
      if( str.length <= len ){
        return str;
      }

      return str.substring(0, len) + "...";
    },

    setScreenPreview: function( elem, screen ){
      console.trace();
      
      elem.style.background = "url("+screen+")";
      elem.style.backgroundSize = "contain";
      elem.style.backgroundPosition = "center center";
      elem.style.backgroundRepeat = "no-repeat";
    },

    setUrlPreview: function( elemParams, picParams ){

      this.Async.chain( [
        function( callback2 ){
          if( !picParams.size ){
            var img = new Image();
            img.onload = function(){
              picParams.size = {
                width: img.width,
                height: img.height
              };

              callback2();
            }
            img.onerror = function(){
              picParams.size = {
                width: 0,
                height: 0
              };


              callback2();
            }

            img.src = picParams.url;
          }
          else{
            callback2();
          }
        },

        function(  ){
          elemParams.elem.style.background = "url("+picParams.url+")";
          elemParams.elem.style.backgroundPosition = "center center";

          if( picParams.size.width < elemParams.size.width && picParams.size.height < elemParams.size.height ){

          }
          else{
            elemParams.elem.style.backgroundSize = "contain";
          }

          elemParams.elem.style.backgroundRepeat = "no-repeat";
        }
      ] );

    },

    imageUrlToDataUrl: function( url, callback, format, quality ){

      var img = new Image();
      img.onload = function(){
        var canvas = document.createElement("canvas");
        var ctx = canvas.getContext('2d');
        canvas.width = img.width;
        canvas.height = img.height;

        ctx.drawImage(img, 0, 0, img.width, img.height);

        if( img.width * img.height < 300*300 ){
          // limitations due to chrome bug
          format = "image/png";
        }

        format = format || "image/jpeg";
        quality = quality || 90;

        callback(  canvas.toDataURL(format, quality) );
      }
      img.onerror = function(){
        callback( null );
      }
      img.src = url;
    },

    getTitleForUrl: function( url, callback ){

      var req = new XMLHttpRequest();
      req.open( "GET", url );
      req.onload = function(){
        var tmp = document.createElement( "div" );
        tmp.innerHTML = req.responseText;
        try{
          var title = tmp.getElementsByTagName( "title" )[0];
          callback( title.textContent );
        }
        catch( ex ){
          callback( null );
        }
      }
      req.onerror = function(){
        callback( null );
      }
      req.send();

    },

    setAutoTextForTextField: function( elem, text ){

      elem.addEventListener( "focus", function(){

        if( elem.hasAttribute( "autoText" ) ){
          elem.removeAttribute( "autoText" );
          elem.value = "";
        }

      }, false );

      elem.addEventListener( "blur", function(){
        if( elem.value == "" ){
          elem.setAttribute( "autoText", 1 );
          elem.value = text;
        }

      }, false );

      if( elem.value == "" ){
        elem.setAttribute( "autoText", 1 );
        elem.value = text;
      }

    },

    Async: {

      lineCallCounter: 0,
      LINE_COUNT_LIMIT: 300,

      chain: function( callbacksChain ){

        var dataObject = {};

        var f = function(){
          if( callbacksChain.length > 0 ){
            var nextCallback = callbacksChain.shift();
            nextCallback( f, dataObject );
          }
        }

        f();

      },

      // simulteneusely process
      sArrayProcess: function( dataArray, callback, finishCallback ){

        var countProcessed = 0;

        if( dataArray.length == 0 ){
          return finishCallback();
        }

        dataArray.forEach(function( item ){

          callback( item, function(){

            countProcessed++;

            if( countProcessed == dataArray.length ){
              finishCallback();
            }

          } );

        });

      },

      arrayProcess: function( dataArray, callback, finishCallback ){

        var that = this;

        var f = function( i ){

          if( i >= dataArray.length ){
            finishCallback();
          }
          else{

            if( that.lineCallCounter > that.LINE_COUNT_LIMIT ){
              setTimeout(function(){

                that.lineCallCounter = 0;

                callback( dataArray[i], function(){
                  f(i + 1);
                } );

              }, 0);
            }
            else{
              that.lineCallCounter++;

              callback( dataArray[i], function(){
                f(i + 1);
              } );
            }

          }

        }

        f(0);

      },

      // controlled asynchronous cicle
      cc: function( stateFunction ){

        var rf = function( result ){

          if( result == "break" ){
            return;
          }

          stateFunction( rf );

        };

        stateFunction( rf );

      }

    },

    UI: {
      showAndHide: function( elem, timeout ){
        timeout = timeout | 3000;
        elem.style.opacity = 1;
        setTimeout( function(){
          elem.style.opacity = 0;
        }, timeout );
      }
    },

    Opener: {
      asClicked: function( url, def, event ){

        var action = def;

        if( event.button == 0 ){
          if( event.ctrlKey ){
            if( event.shiftKey ){
              action = "background";
            }
            else{
              action = "new";
            }
          }
        }
        else if( event.button == 1 ){
          action = "background";
        }

        this.byAction( action, url );

      },

      byAction: function( action, url ){
        switch( action ){
          case "current":
            this.currentTab( url );
          break;
          case "new":
            this.newTab( url );
          break;
          case "background":
            this.backgroundTab( url );
          break;
        }
      },

      currentTab: function( url ){
        chrome.tabs.getCurrent(function( tab ){
          chrome.tabs.update( tab.id, {
            url: url
          } );
        })
      },

      newTab: function( url ){
        chrome.tabs.create({
          url: url,
          active: true
        });
      },

      backgroundTab: function( url ){
        chrome.tabs.create({
          url: url,
          active: false
        });
      }
    }
  };


  this.Utils = new Utils();


  (function(){

    var DD = function(){

    }

    var _dragAndDropElem = function( params ){
      var that = this;

      this._elem = params.elem;
      this._ddTargets = params.targets;
      this._initParams = params;
      this._lastMousePos = null,

      // methods
      this.event = function( type, params ){
        if( that._initParams["callback" + type] ){
          that._initParams["callback" + type](params);
        }
      }

      this.init = function(){

        this._elem.addEventListener( "mousedown", function( event ){

          if( event.button != 0 ){
            return;
          }

          that._draggingStartCursorPosition = that._mousePos( event );

          document.addEventListener( "mousemove", that._mouseMove, false );
          document.addEventListener( "mouseup", that._mouseUp, false );

        }, false );


      }

      this.adjustPos = function( mouse ){

        mouse = mouse || that._lastMousePos;
        that._lastMousePos = mouse;

        var marginLeft = mouse.x - that._draggingStartCursorPosition.x;
        var marginTop = mouse.y - that._draggingStartCursorPosition.y;

        that._elem.style.webkitTransition = "none";

        var newMargins = that._initParams.changePos( marginLeft, marginTop, that );
        marginLeft = newMargins.left;
        marginTop = newMargins.top;

        if( marginLeft !== false ){
          that._elem.style.marginLeft = marginLeft + "px";
        }

        if( marginTop !== false ){
          that._elem.style.marginTop = marginTop + "px";
        }

        var elemOffset = fvdSpeedDial.Utils.getOffset( that._elem );

        var centerPos = {
          left: elemOffset.left + that._elem.offsetWidth/2,
          top: elemOffset.top + that._elem.offsetHeight/2
        };

        var nowDraggedOn = [];

        for( var i = 0; i != that._ddTargetsList.length; i++ ){
          var targetOffset = fvdSpeedDial.Utils.getOffset( that._ddTargetsList[i] );

          if( centerPos.left >= targetOffset.left && centerPos.left <= targetOffset.left + that._ddTargetsList[i].offsetWidth &&
              centerPos.top >= targetOffset.top && centerPos.top <= targetOffset.top + that._ddTargetsList[i].offsetHeight ){

            nowDraggedOn.push( that._ddTargetsList[i] );

          }
        }

        for( var i = 0; i != nowDraggedOn.length; i++ ){
          if( that._nowDraggedOn.indexOf(nowDraggedOn[i]) == -1 ){
            that.event( "Dragon", nowDraggedOn[i] );
          }
        }

        for( var i = 0; i != that._nowDraggedOn.length; i++ ){
          if( nowDraggedOn.indexOf(that._nowDraggedOn[i]) == -1 ){
            that.event( "Dragleave", that._nowDraggedOn[i] );
          }
        }

        that._nowDraggedOn = nowDraggedOn;

        return {
          left: marginLeft,
          top: marginTop
        };

      },

      this._mouseMove = function( event ){

        if( !that._nowDragging ){
          that._nowDragging = true;
          // search elements for drag
          var targets = document.querySelectorAll( "*[dd_class~="+that._ddTargets+"]" );
          that._ddTargetsList = [];
          for( var i = 0; i != targets.length; i++ ){
            if( targets[i] == that._elem ){
              continue;
            }
            that._ddTargetsList.push( targets[i] );
          }

          that.event("MouseDown");
        }

        var mouse = that._mousePos(event);

        var margins = that.adjustPos( mouse );

        if( !that._startEventSent ){
          if( margins.left != 0 || margins.top != 0 ){
            that._startEventSent = true;
            that.event( "Start" );
          }
        }

      }

      this._mouseUp = function( event ){
        document.removeEventListener( "mousemove", that._mouseMove, false );
        document.removeEventListener( "mouseup", that._mouseUp, false );

        that._elem.style.webkitTransition = "";
        that._elem.style.marginLeft = "";
        that._elem.style.marginTop = "";
        that._nowDragging = false;
        that._startEventSent = false;

        that.event( "End", {elements: that._ddTargetsList} );
      }

      this._mousePos = function( event ){
        var scrollTop = document.body.scrollTop;
        if( this._initParams.scrollingNotMean ){
          scrollTop = 0;
        }

        return {
          x: event.x,
          y: event.y + scrollTop
        };
      }

      this.init();


    }

    _dragAndDropElem.prototype = {
      // options
      _initParams: null,
      _elem: null,
      _ddTargets: null,

      // privates
      _ddTargetsList: null,
      _nowDragging: false,
      _draggingStartCursorPosition: {x:null, y:null},
      _nowDraggedOn: [],
      _startEventSent: false,

    }

    DD.prototype = {

      create: function( params ){
        return new _dragAndDropElem( params );
      }

    }

    this.DD = new DD();

  }).apply(this.Utils);

}).apply( fvdSynchronizer );

