(function(){
  var FS_SIZE = 500 * 1024 * 1024;
  var FS_TYPE = window.PERSISTENT;
	var FileSystem = function(){

		var self = this;

		function request(callback){
			webkitRequestFileSystem(FS_TYPE, FS_SIZE, function(fs){

				callback(fs);

			});
		}

		function init(){

			request( function( fs ){

				fs.root.getDirectory( "backups", {create: true}, function( dir ){
					dir.getDirectory( "bookmarks", {create: true}, function(){} );
					dir.getDirectory( "sd", {create: true}, function(){} );
				} );

			} );

		}

    this.getFiler = function(cb) {
      var filer = new Filer();
      filer.init({
        size: FS_SIZE,
        persistent: FS_TYPE === window.PERSISTENT
      }, function() {
        cb(filer)
      });
    };

		this.dirContents = function( path, callback ){

			request( function(fs) {

				function toArray(list) {
					return Array.prototype.slice.call(list || [], 0);
				}

				fs.root.getDirectory( path, {create: true}, function(dir) {

					var dirReader = dir.createReader();
					var entries = [];

					// Keep calling readEntries() until no more results are returned.
					var readEntries = function() {
						dirReader.readEntries (function(results) {

							if (!results.length) {
								entries.sort();

								var names = [];
								entries.forEach(function( entry ){
									names.push( entry.name.replace( path, "" ) );
								});

								callback( names );
							} else {
								entries = entries.concat(toArray(results));
								readEntries();
							}

						}, function(err) {
              console.error("Fail read dir contents", path, err);
							callback( false );
						});
					};

					readEntries();

				}, function(){
					callback(false);
				});

			} );

		}

		this.makeDir = function( path, callback ){

			request(function( fs ){

				fs.root.getDirectory( path, {create: true}, function( dir ) {

					callback();

				});

			});

		}

		this.removeDir = function( path, callback ){

			request(function( fs ){

				fs.root.getDirectory( path, {create: true}, function( dir ){

					dir.removeRecursively( function(){
						callback( true) ;
					}, function() {
            console.error("Fail remove dir recursively", path, err);
						callback( false );
					} );

				});

			});

		}

		this.read = function(name, callback){

			request(function(fs) {

				fs.root.getFile(name, null, function(fileEntry) {

					fileEntry.file(function(f) {
						var reader = new FileReader();

						reader.onload = function(e){
							callback( true, e.target.result );
						}
            reader.onerror = function(err) {
              console.error("Fail to read file", name, err);
            };

						reader.readAsText(f);

					});

				}, function(err) {
          console.error("Error getting file", err);
					callback(false);
				});

			});

		}

		this.truncate = function(name, callback){

			request(function( fs ){

				fs.root.getFile(name, {
					create: true
				}, function(fileEntry){

					fileEntry.createWriter(function(f){
						f.onwriteend = function(event){
							callback(true);
						};
						f.onerror = function(err) {
              console.error("Fail truncate file", name, err);
						};

						f.truncate(0);
					});

				});

			});

		}

		this.write = function(name, text, callback){

			self.truncate( name, function(){

				request(function( fs ){

					fs.root.getFile(name, {
						create: true
					}, function(fileEntry){

						fileEntry.createWriter(function(f){
							f.onwriteend = function(event) {
                console.log("Done write file", name);
								callback(true);
							};
							f.onerror = function(err){
                console.error("Fail write file", name, err);
							};
							var blob = new Blob( [text] );
							f.write( blob );
						});

					});

				});

			} );

		}

		this.exists = function( name, callback ) {

			request(function(fs){
				fs.root.getFile(name, null, function(fileEntry){
					callback( true );
				}, function(){
					callback( false );
				});
			});

		};

		document.addEventListener( "DOMContentLoaded", function(){
			init();
		}, false );

	}

	fvdSynchronizer.FileSystem = new FileSystem();

})();
