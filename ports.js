

function subscribe(app) {

	window.onmousedown = function(e) {
	  app.ports.windowMouseDown.send(e)
	}

	window.onmouseup = function(e) {
	  app.ports.windowMouseUp.send(e)
	}


		window.onmousemove = function(e) {
	    app.ports.windowMouseMove.send(e)
	  }


		window.onmousewheel = function(e) {
	    app.ports.windowMouseWheel.send(e)
	  }


		window.onfocus = function() {
	    app.ports.windowFocus.send(true)
	  }

	  window.onblur = function() {
	    app.ports.windowFocus.send(false)
	  }


		app.ports.askGeometry.subscribe(function(id) {
        var elem = document.getElementById(id)
        app.ports.clientRect.send([id, elem.getBoundingClientRect()]);
    });

}
