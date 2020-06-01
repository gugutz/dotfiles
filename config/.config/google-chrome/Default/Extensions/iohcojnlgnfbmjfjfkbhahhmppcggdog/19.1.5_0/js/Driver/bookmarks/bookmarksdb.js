var bookmarksDatabase = new function() {
  var systemGuids = [
    "menu",
    "unsorted",
    "toolbar"
  ];

  var self = this;

  function generateGUID() {
    var chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghiklmnopqrstuvwxyz";
    var string_length = 32;
    var randomstring = '';
    for (var i=0; i<string_length; i++) {
      var rnum = Math.floor(Math.random() * chars.length);
      randomstring += chars.substring(rnum,rnum+1);
    }
    return randomstring;
  }

  this.systemGuids = function(){
    return systemGuids;
  };

  this.getAllGuids = function(){
    return BookmarksGuidsStorage.getAllGuids();
  };

  this.getAllIds = function(){
    return BookmarksGuidsStorage.getAllIds();
  };

  this.getGuid = function(id){
    return BookmarksGuidsStorage.getGuid( id );
  };

  this.getCountStoredGuids = function(){
    return BookmarksGuidsStorage.getGuidsCount();
  };

  this.getId = function(guid){
    return BookmarksGuidsStorage.getId( guid );
  };

  this.setGuid = function(id, guid){
    guid = guid || generateGUID();
    BookmarksGuidsStorage.setGuid( id, guid );
    return  guid;
  };

  this.removeGuid = function(id){
    BookmarksGuidsStorage.removeGuidById( id );
  };

  this.startMassGuidsChange = function(){
    BookmarksGuidsStorage.startMassGuidsChange();
  };

  this.applyMassGuidsChange = function(){
    BookmarksGuidsStorage.applyMassGuidsChange();
  };

  this.rollbackMassGuidChange = function(){
    BookmarksGuidsStorage.cancelMassGuidsChange();
  };

  this.connect = function(callback) {
    callback();
  };

}();