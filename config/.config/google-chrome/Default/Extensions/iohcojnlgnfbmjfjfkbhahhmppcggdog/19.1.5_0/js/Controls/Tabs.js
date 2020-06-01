(function(){
	
	var self = this;
	
	this.Tabs = {
		
		tabs: [],
		tabsByIds: {},
		
		_initialized: false,
		
		_initCallbacks: [],
		
		onChange: new EventEmitter(),
		
		_createInstance: function( tabsBox ){
			
			var TabBox = function( tabsBox ){				
				
				this.setTab = function( num ){
					setActiveTab( num );
				}
				
				this.countActiveTabs = function(){
					var active = 0;
					
					var h = tabsHeads();
										
					for( var i = 0; i != h.length; i++ ){
						if( !isTabActive( h[i] ) ){
							continue;
						}
						
						active++;
					}
					
					return active;
				}
				
				this.refreshActiveTab = function(){
					
					try{
						if( !isTabActive( this.getActiveTabElem() ) ){
							
							setActiveTab( 0 );
							
						}
						
					}
					catch( ex ){
						
					}
					
				}
				
				this.getActiveTabElem = function(){
					return document.querySelector( "#" + tabsBox.getAttribute("id") + " > .tabsHead > .tabHead[active]" );
				}
				
				function isTabActive( tab ){
					if( tab.style.display == "none" || tab.hasAttribute("hidden") ){
						return false;
					}
					
					return true;
				}
				
				function tabsContent(){
					return document.querySelectorAll( "#" + tabsBox.getAttribute("id") + " > .tabsData > .tabContent" );
				}
				
				function tabsHeads(){
					return document.querySelectorAll( "#" + tabsBox.getAttribute("id") + " > .tabsHead > .tabHead" );
				}
				
				function setActiveTab( tabNum, swallow ){
					
				 	if( !swallow ){
						fvdSynchronizer.Controls.Tabs.onChange.callListeners( tabsBox, tabNum );						
					}
					
					var heads = tabsHeads();
					var contents = tabsContent();
					
					for( var i = 0; i != heads.length; i++ ){							
						if( i == tabNum ){
							heads[i].setAttribute( "active", 1 );
						}
						else{
							heads[i].removeAttribute( "active", 1 );
						}							
					}
					
					for( var i = 0; i != contents.length; i++ ){
						if( i == tabNum ){
							contents[i].style.display = "block";
						}
						else{
							contents[i].style.display = "none";
						}
					}
					
				}
				
				var heads = tabsHeads();
				
				for( var i = 0; i != heads.length; i++ ){
					
					(function(i){
						heads[i].addEventListener( "click", function( event ){
							
							if( event.button != 0 ){
								return;
							}
							
							setActiveTab( i );
							
						}, false );
					})(i);						

				}
				
				setActiveTab(0, true);
				
			}
			
			return new TabBox( tabsBox );
			
		},		
	
		
		init: function(){
			
			var tabs = document.getElementsByClassName( "tabs" );
			
			for( var i = 0; i != tabs.length; i++ ){
				
				var inst =  this._createInstance( tabs[i] );
				this.tabs.push( inst );
				
				if( tabs[i].id ){
					this.tabsByIds[tabs[i].id] = inst;
				}
				
			}
			
			this._initialized = true;
			
			this._initCallbacks.forEach(function( callback ){
				callback();
			});
			
			
		},
		
		addInitCallback: function( callback ){
			
			if( this._initialized ){
				callback();
				return;
			}
			
			this._initCallbacks.push( callback );
			
		}
		
	}
	
	document.addEventListener( "DOMContentLoaded", function(){
		
		self.Tabs.init();
		
	}, false );
	
}).apply( fvdSynchronizer.Controls );
