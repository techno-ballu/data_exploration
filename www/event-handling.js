  // Put code in an Immediately Invoked Function Expression (IIFE).
  // This isn't strictly necessary, but it's good JavaScript hygiene.
 $(document).ready(function() {
    
  //      document.getElementById("dwnldBtn").onclick = function() {
  //       debugger
  //       var doctype = '<?xml version="1.0" standalone="no"?><!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">';
        
  //      var x = document.getElementById("x").value;
  //      var y = document.getElementById("y").value;
  //      var outer = document.createElement("div");
  //      el = d3.select("svg.nvd3-svg").node(); //d3.select("svg").node();
  //      var clone = el.cloneNode(true);
       
  //      var xmlns = "http://www.w3.org/2000/xmlns/";
  //      clone.setAttribute("version", "1.1");
  //      clone.setAttributeNS(xmlns, "xmlns", "http://www.w3.org/2000/svg");
  //      clone.setAttributeNS(xmlns, "xmlns:xlink", "http://www.w3.org/1999/xlink");
  //      clone.setAttribute("height", 400);
  //      clone.setAttribute("width", 900);
       
  //      clone.setAttribute("preserveAspectRatio", "none");
  //      outer.appendChild(clone);
        
  //       var sheets = document.styleSheets;
  //       var used = "";
  //       for (var i = 0; i < sheets.length; i++) {
  //         var rules = sheets[i].cssRules;
  //         for (var j = 0; j < rules.length; j++) {
  //           var rule = rules[j];
  //           if (typeof(rule.style) != "undefined") {
  //             var elems = document.querySelectorAll(rule.selectorText);
  //             if (elems.length > 0) {
  //               used += rule.selectorText + " { " + rule.style.cssText + " }\n";
  //             }
  //           }
  //         }
  //       }
        
  //       var s = document.createElement('style');
  //       s.setAttribute('type', 'text/css');
  //       s.innerHTML = "<![CDATA[\n" + used + "\n]]>";
        
  //       var defs = document.createElement('defs');
  //       defs.appendChild(s);
  //       clone.insertBefore(defs, clone.firstChild);
       
  //       var svg = doctype + outer.innerHTML;
  //       var uri = 'data:image/svg+xml;base64,'+ btoa(unescape(encodeURIComponent(svg))); 
        
  //       var img = '<img id="image" src="'+uri+'">'; 
  //       //$("body").append(img);
  
         
  //       var image = new Image();
  //       image.src = uri;
  //       image.onload = function() {
  //          var canvas = document.createElement('canvas');
  //          var w = document.createAttribute("width");
  //          w.value = image.width;
  //          canvas.setAttributeNode(w);
  //          var h = document.createAttribute("height");
  //          h.value = image.height;
  //          canvas.setAttributeNode(h);
           
  //          var context = canvas.getContext('2d');
  //          context.drawImage(image, 0, 0);

  //          var a = document.createElement('a');
  //          a.download = x + "_" + y + ".png";
  //          a.href = canvas.toDataURL('image/png')
  //          document.body.appendChild(a);
  //          a.click();
  //       }
  //     }
      
         
     $(document).on("click", "a.export", function(evt, name) {
       var el = $(evt.target);
       var feature = {
            f1 : document.getElementById("x").value,
            f2 : document.getElementById("y").value
          }
          var result = {
           features : feature,
           type : "scatter",
           random  : Math.random()  
          }
        Shiny.onInputChange("dwnldData1", result);      
      })
      
      Shiny.addCustomMessageHandler("myCallbackHandler",
       function(message) {
        //alert(JSON.stringify(message));
     })
  });



      /**
      * Saves a chart svg in physical drive
      **/
      var saveChartModule = (function(){
        var doctype = '<?xml version="1.0" standalone="no"?><!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www .w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">';
        
        var xmlns = "http://www.w3.org/2000/xmlns/";
        var version = "1.1";
        
        //creates a clone of html
        cloneEl = function(el){
           return el.cloneNode(true);
        };
        
        //adds attributes to a html tag
        addAtrributes = function(clone, h, w){
          clone.setAttribute("version", version);
          clone.setAttributeNS(xmlns, "xmlns", "http://www.w3.org/2000/svg");
          clone.setAttributeNS(xmlns, "xmlns:xlink", "http://www.w3.org/1999/xlink");
          clone.setAttribute("height", h);
          clone.setAttribute("width", w);
          return clone;
        };
        
        //creates a wrapper div to get the innerHTML
        createOuter = function(children){
          var outer = document.createElement("div");
          outer.appendChild(children);
          return outer;
        }
        
        //gets the svg content
        getSvg = function(el, h, w){
          var clonedEl = cloneEl(el);
          clonedEl = addAtrributes(clonedEl, h, w);
          clonedEl = createStyles(clonedEl);
          var outer = createOuter(clonedEl);
          return doctype + outer.innerHTML;
         }
        
        //appends the stylesheets in a page
        createStyles = function(clone){
          var sheets = document.styleSheets;
          var used = "";
          for (var i = 0; i < sheets.length; i++) {
              var rules = sheets[i].cssRules;
              for (var j = 0; j < rules.length; j++) {
                var rule = rules[j];
                if (typeof(rule.style) != "undefined") {
                    var elems = document.querySelectorAll(rule.selectorText);
                    if (elems.length > 0) {
                      used += rule.selectorText + " { " + rule.style.cssText + " }\n";
                    }
                }
              }
            }
              
            var s = document.createElement('style');
            s.setAttribute('type', 'text/css');
            s.innerHTML = "<![CDATA[\n" + used + "\n]]>";
              
            var defs = document.createElement('defs');
            defs.appendChild(s);
            clone.insertBefore(defs, clone.firstChild);
            return clone;
        }
        
        //converts a html input to encoded uri
        svgAsDataUri = function(svg){
          var uri = 'data:image/svg+xml;base64,'+ btoa(unescape(encodeURIComponent(svg)));
          return uri;
        }
        
        //saves a chart as PNG
        saveSvgAsPng = function(uri, chartname){
          var image = new Image();
          image.src = uri;
          image.onload = function() {
             var canvas = document.createElement('canvas');
             var w = document.createAttribute("width");
             w.value = image.width;
             canvas.setAttributeNode(w);
             var h = document.createAttribute("height");
             h.value = image.height;
             canvas.setAttributeNode(h);
             
             var context = canvas.getContext('2d');
             context.drawImage(image, 0, 0);
      
             var a = document.createElement('a');
             a.download = chartname +  ".png";
             a.href = canvas.toDataURL('image/png')
             document.body.appendChild(a);
             a.click();
          }
        }
        
        return {
           saveChart : function(chartname, el, h, w){
            var svg = getSvg(el, h, w);
            var uri = svgAsDataUri(svg);
            saveSvgAsPng(uri, chartname);
          }
        };
        
      })();


    function saveChart(chartname, el, h, w){
       saveChartModule.saveChart(chartname, el, h, w);
    }
    
    var dwnldModule1 = {
       saveHTML : $("<button id='save type='button' class='btn btn-default'>Save As Image</button>"),
       exportHTML : $("<button id='export' type='button' class='btn btn-default'>Export Data</button>"),
       css : 'btn-group-xs pull-right ',
       
       getDivID : function(event){
         var hash = "download_panel";
          if(event != null)
           return event.concat("-").concat(hash);
       },
       getSelector : function(){
            return d3.select("." + this.id).node();
       },
       createContainer : function(div, el, id){
          if(div == null){
           div = "<div class='" + this.css + id + "'></div>";
           $(el).before(div);
           container = d3.select("div." + id).node();
           $(container).append(this.saveHTML);
            //$(div).append(this.exportHTML);
          }
          //return  $(div);
       },
       destroy : function(event, el){
      }
    }
    
    var dwnldModule = (function(){
        var id = 'download_panel';
        var saveHTML = $("<button id='save type='button' class='btn btn-default'>Save As Image</button>");
        var exportHTML = $("<button id='export' type='button' class='btn btn-default'>Export Data</button>");
        var css = 'btn-group-xs pull-right ';
        
        getDivID = function(event){
          if(event != null)
           id = event.concat("-").concat(id);
           return id;
        }
        getSelector = function(id){
            return d3.select("." + id).node();
        }
        createContainer = function(container, el){
          if(container == null){
            css += id;
            dwnldPanelHTML = "<div class='" + css + "'></div>";
            $(el).before(dwnldPanelHTML);
          }
          return getSelector(id);
        }
        return {
          createButtons : function(event, el){
            id = getDivID(event);
            dwnldPanel = getSelector(id);
            dwnldPanel = createContainer(dwnldPanel, el);
            $(dwnldPanel).append(saveHTML);
            $(dwnldPanel).append(exportHTML);
          }
        };
    })();
    
    function addDwnloadButtonsInSVG(event, el){
     // var id = dwnldModule1.getDivID(event);
     // var container = dwnldModule1.getSelector(id);
     /// container = dwnldModule1.createContainer(container, el, id);
      
      var id = 'download_panel';
      if(event != null){
        id = event.concat("-").concat(id);
      }
      var dwnldPanel = d3.select("." + id).node();
      if(dwnldPanel == null){
         var cls = "btn-group-xs pull-right ";
         cls += id;
         dwnldPanelHTML = "<div class='" + cls + "'></div>";
         $(el).before(dwnldPanelHTML)
         var save = $("<button id='save' type='button' class='btn btn-default'>Save As Image</button>");
         var exportBtn = $("<button id='export' type='button' class='btn btn-default'>Export Data</button>");
         dwnldPanel = d3.select("." + id).node();
         $(dwnldPanel).append(save);
         $(dwnldPanel).append(exportBtn);
      }
      return id;
  }
function transformHandler(name,chooserID){    
         var result = {
         feature : name,
         chooser : chooserID,
         random  : Math.random()  
        }
        Shiny.onInputChange("transformData", result);        
}

// Save button handler
function saveTransformHandler(name){    
       var result = {
       feature : name,
       random  : Math.random()  
      }
      Shiny.onInputChange("saveTransformedData", result);
}

function updateChooser_(chooser) {
    chooser = $(chooser);
    var left = chooser.find("select.left");
    var right = chooser.find("select.right");
    var leftArrow = chooser.find(".left-arrow");
    var rightArrow = chooser.find(".right-arrow");
    
    var canMoveTo = (left.val() || []).length > 0;
    var canMoveFrom = (right.val() || []).length > 0;
    
    leftArrow.toggleClass("muted", !canMoveFrom);
    rightArrow.toggleClass("muted", !canMoveTo);
}


// Reset button Handler
function resetHandler(name,chooserID){  
  move_($("#"+chooserID+" .right-arrow").parents(".chooser"),".right", ".left" )  
    var result = {
         feature : name,         
         random  : Math.random()  
   }
  Shiny.onInputChange("resetData", result);      
}




function move_(chooser, source, dest) {
    chooser = $(chooser);    
    var selected = chooser.find(source).children("option");
    var dest = chooser.find(dest);
    dest.children("option").each(function(i, e) {e.selected = false;});
    dest.append(selected);
    updateChooser_(chooser);
    chooser.trigger("change");
}

